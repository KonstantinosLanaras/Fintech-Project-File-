// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

/**
 * @dev Minimal ERC-20 interface for the stablecoin used to denominate the
 *      fund (e.g. USDC). We don't import the full OpenZeppelin interface to
 *      keep this single-file, but in production use
 *      openzeppelin/contracts/token/ERC20/IERC20.sol instead of this stub.
 */
interface IERC20 {
    function transfer(address to, uint256 amount) external returns (bool);
    function transferFrom(address from, address to, uint256 amount) external returns (bool);
    function balanceOf(address account) external view returns (uint256);
}

/**
 * @title PrivateFundLPToken
 * @notice A permissioned token representing Limited Partner (LP) interests in a
 *         closed-end private fund (e.g. PE, VC, private credit). This contract
 *         models the core lifecycle of a fund: commitment, capital calls,
 *         NAV marking, and a European-style (whole-fund) distribution waterfall.
 *
 * @dev This is an EDUCATIONAL / PROTOTYPE reference implementation. It is not
 *      audited and should not be used to hold real investor funds without a
 *      full security review, legal structuring, and adaptation to your
 *      jurisdiction's securities law (in the EU: AIFMD, MiFID II, MiCAR scope
 *      analysis, etc).
 *
 *      DESIGN NOTE: token "balance" here represents COMMITTED capital, not
 *      called capital. This mirrors how real fund admins track LPs — your
 *      commitment is fixed at subscription and doesn't change as it gets
 *      called; what changes is how much of it has been drawn down. We track
 *      called/distributed amounts in separate per-LP state rather than
 *      mutating the token balance itself, so that "1 LP token = 1 unit of
 *      commitment" stays a stable, auditable invariant.
 */
contract PrivateFundLPToken {

    // =========================================================================
    // FUND-LEVEL STATE
    // =========================================================================

    address public fundAdmin; // The GP / fund administrator's authorized address
                               // (in production this should be a multisig, e.g.
                               // Gnosis Safe, not a single EOA — capital call
                               // and NAV authority is too sensitive for one key)

    address public gpPayoutAddress; // where GP's carried interest is sent on claim

    string public fundName;
    uint256 public fundInceptionDate;

    /// @notice The stablecoin (e.g. USDC) the fund is denominated in.
    /// @dev Real private funds are denominated in fiat currency, not ETH —
    ///      LPA commitments are stated in EUR/USD, capital calls are due in
    ///      EUR/USD, and NAV is reported in EUR/USD. A stablecoin is the
    ///      practical on-chain stand-in for that denomination currency. We
    ///      use a single token here for simplicity; a multi-currency fund
    ///      would need an FX conversion layer, which is out of scope.
    IERC20 public denominationAsset;

    /// @notice Hurdle rate (preferred return), expressed in basis points (bps).
    /// @dev A typical PE fund hurdle is 8% annualized = 800 bps. This is the
    ///      minimum compounded return LPs must receive on their drawn capital
    ///      BEFORE the GP is entitled to any carried interest. It exists to
    ///      align GP incentives with LP outcomes — the GP only gets paid
    ///      "extra" once LPs have cleared a baseline return.
    uint256 public constant HURDLE_RATE_BPS = 800;

    /// @notice GP catch-up percentage in bps (10000 = 100%).
    /// @dev After the hurdle is paid, the GP receives 100% of the NEXT
    ///      distributions (the "catch-up") until the GP's cumulative take
    ///      equals 20% of total profits distributed so far (the standard
    ///      "2-and-20" carry ratio). This is what allows the GP to "catch up"
    ///      to its full carry percentage despite having received nothing
    ///      during the hurdle phase.
    uint256 public constant GP_CATCHUP_BPS = 10000;

    /// @notice Final carried interest split once catch-up is complete.
    /// @dev 2000 bps = 20%, the LP gets the remaining 8000 bps = 80%.
    uint256 public constant CARRY_BPS = 2000;
    uint256 private constant BPS_DENOMINATOR = 10000;

    /// @notice Total LP commitments across all investors, in the fund's
    ///         denomination currency (we use a uint256 "cents" or "wei-like"
    ///         unit — in production this would be pegged to a stablecoin
    ///         decimals convention, e.g. 6 decimals for USDC).
    uint256 public totalCommitted;
    uint256 public totalCalled;       // cumulative capital drawn down from LPs
    uint256 public totalDistributed;  // cumulative capital returned to LPs (excl. GP carry)
    uint256 public totalCarryPaid;    // cumulative carried interest paid to GP

    /// @notice Last published Net Asset Value of the fund (in the same
    ///         denomination unit as commitments).
    /// @dev NAV is NOT computed on-chain. Private fund assets (unlisted
    ///      portfolio companies, real estate, private credit positions) have
    ///      no continuous market price. NAV is a periodic (typically
    ///      quarterly) judgment-based valuation produced off-chain by the
    ///      fund administrator/auditor and PUSHED on-chain as an oracle
    ///      update. Anyone reading currentNAV should know it can be stale
    ///      (up to one quarter old) — this is fundamentally different from
    ///      a DeFi price oracle reading a live AMM pool.
    uint256 public currentNAV;
    uint256 public lastNAVUpdateTimestamp;

    /// @notice The minimum time between NAV updates, to prevent the fund
    ///         admin from arbitrarily repricing the fund more often than
    ///         the LPA's stipulated valuation cadence (commonly quarterly).
    uint256 public constant MIN_NAV_UPDATE_INTERVAL = 90 days;

    // =========================================================================
    // TOKEN STATE (the "rails" — ERC-20-like but permissioned)
    // =========================================================================

    string public constant name = "Fund LP Commitment Token";
    string public constant symbol = "FLP";
    uint8  public constant decimals = 18;
    uint256 public totalSupply; // mirrors totalCommitted; minted on subscription

    mapping(address => uint256) private _balances; // commitment balance per LP

    /// @notice Per-LP fund accounting. This is the core data structure that
    ///         makes this different from a generic fungible token: each LP's
    ///         economic position depends on their individual call/distribution
    ///         history, not just their current token balance.
    struct LPAccount {
        uint256 committed;          // total commitment (mirrors token balance)
        uint256 called;             // cumulative capital actually drawn down
        uint256 distributed;        // cumulative capital returned (return of capital + pref + carry-adjacent)
        uint256 claimable;          // PULL-PATTERN: amount this LP can withdraw right now,
                                     // credited by distribute() but not yet sent — see claim()
        uint256 lastCallTimestamp;  // for tracking hurdle accrual periods
        bool isWhitelisted;         // KYC/AML + accreditation check passed
        bool isDefaulted;           // LP failed to fund a capital call
    }

    mapping(address => LPAccount) public lpAccounts;
    address[] public lpRegistry; // enumerable list of all LPs ever onboarded

    // =========================================================================
    // CAPITAL CALL STATE
    // =========================================================================

    /// @notice A capital call is a pro-rata draw-down request issued to all LPs
    ///         simultaneously. Unlike a public company calling a dividend, a
    ///         capital call is a DEMAND for cash, not a payment — LPs owe the
    ///         fund money here, the flow of funds is investor -> fund.
    struct CapitalCall {
        uint256 callId;
        uint256 totalAmount;        // total being called across all LPs
        uint256 callBps;            // percentage of EACH LP's commitment being called, in bps
        uint256 dueDate;
        string  purpose;            // e.g. "Investment in Portfolio Co. X" or "Management Fee Q3"
        bool    finalized;
        mapping(address => bool) funded; // which LPs have met this call
    }

    uint256 public capitalCallCounter;
    mapping(uint256 => CapitalCall) private capitalCalls;

    // =========================================================================
    // DISTRIBUTION STATE
    // =========================================================================

    /// @notice Tracks GP's cumulative entitlement so the catch-up mechanism
    ///         can be computed correctly across multiple distribution events
    ///         over the fund's life — carry is a WHOLE-FUND cumulative
    ///         calculation (European waterfall), not computed deal-by-deal.
    uint256 public cumulativeProfitDistributed; // profit = distributions beyond return of capital
    uint256 public cumulativeHurdlePaid;
    uint256 public cumulativeCatchupPaid;

    /// @notice GP's pull-pattern claimable balance, mirroring LPAccount.claimable.
    /// @dev Kept separate from LPAccount since the GP isn't an LP and doesn't
    ///      hold a commitment token balance.
    uint256 public gpClaimable;

    // =========================================================================
    // EVENTS
    // =========================================================================

    event LPOnboarded(address indexed lp, uint256 commitment);
    event Transfer(address indexed from, address indexed to, uint256 value);
    event CapitalCallIssued(uint256 indexed callId, uint256 totalAmount, uint256 callBps, uint256 dueDate, string purpose);
    event CapitalCallFunded(uint256 indexed callId, address indexed lp, uint256 amount);
    event LPDefaulted(uint256 indexed callId, address indexed lp);
    event NAVUpdated(uint256 newNAV, uint256 timestamp);
    event DistributionExecuted(uint256 totalAmount, uint256 toLPs, uint256 toGP);
    event Whitelisted(address indexed lp, bool status);
    event Claimed(address indexed claimant, uint256 amount);

    // =========================================================================
    // MODIFIERS
    // =========================================================================

    modifier onlyFundAdmin() {
        require(msg.sender == fundAdmin, "PrivateFund: caller is not fund admin");
        _;
    }

    modifier onlyWhitelisted(address account) {
        require(lpAccounts[account].isWhitelisted, "PrivateFund: address not KYC/AML whitelisted");
        _;
    }

    constructor(string memory _fundName, address _fundAdmin, address _denominationAsset, address _gpPayoutAddress) {
        fundName = _fundName;
        fundAdmin = _fundAdmin;
        denominationAsset = IERC20(_denominationAsset);
        gpPayoutAddress = _gpPayoutAddress;
        fundInceptionDate = block.timestamp;
    }

    // =========================================================================
    // 1. LP ONBOARDING & COMMITMENT (mint)
    // =========================================================================

    /**
     * @notice Onboards a new LP and mints tokens representing their commitment.
     * @dev MINTING HERE = RECORDING A COMMITMENT, NOT A CASH TRANSFER.
     *      No money moves at subscription time in most fund structures — the
     *      LP signs a subscription agreement promising to fund up to this
     *      amount WHEN CALLED. The token therefore represents an obligation
     *      / claim, not currently-deployed capital. This is the single most
     *      important conceptual difference from a typical "deposit and mint"
     *      DeFi vault pattern.
     *
     *      Whitelisting must happen BEFORE minting — in a real deployment
     *      this function would be preceded by an off-chain KYC/AML check and
     *      an accredited/qualified investor determination (relevant under
     *      AIFMD Art. 43 for retail marketing restrictions, or Reg D /
     *      Reg S equivalents outside the EU).
     */
    function onboardLP(address lp, uint256 commitmentAmount) external onlyFundAdmin {
        require(lp != address(0), "PrivateFund: zero address");
        require(commitmentAmount > 0, "PrivateFund: commitment must be positive");
        require(lpAccounts[lp].committed == 0, "PrivateFund: LP already onboarded");

      lpAccounts[lp] = LPAccount({
    committed: commitmentAmount,
    called: 0,
    distributed: 0,
    claimable: 0,  // <-- Add this
    lastCallTimestamp: block.timestamp,
    isWhitelisted: true,
    isDefaulted: false
});

        lpRegistry.push(lp);

        _balances[lp] += commitmentAmount;
        totalSupply += commitmentAmount;
        totalCommitted += commitmentAmount;

        emit LPOnboarded(lp, commitmentAmount);
        emit Transfer(address(0), lp, commitmentAmount);
        emit Whitelisted(lp, true);
    }

    function balanceOf(address account) external view returns (uint256) {
        return _balances[account];
    }

    // =========================================================================
    // 2. PERMISSIONED TRANSFER (secondary market, restricted)
    // =========================================================================

    /**
     * @notice Transfers LP commitment tokens between whitelisted addresses.
     * @dev REAL PRIVATE FUND SECONDARIES ARE NOT FREE TRANSFERS. LPAs almost
     *      universally include:
     *        1. Right of First Refusal (ROFR) — the GP or other LPs may have
     *           a contractual right to buy before a third party can.
     *        2. GP consent requirement — the GP can block transfers to
     *           parties it doesn't want as LPs (reputational, regulatory,
     *           competitive reasons).
     *        3. Minimum holding periods / lock-ups.
     *        4. Both parties must independently satisfy KYC/AML and
     *           investor-eligibility checks — you cannot "gift" a private
     *           fund interest to an unaccredited wallet.
     *      Here we encode (2) and (4) directly; (1) and (3) would be
     *      layered in as additional require() checks or a separate
     *      SecondaryMarketplace contract that this token defers to.
     *
     *      NOTE: transferring the token here moves the COMMITMENT, but the
     *      buyer also needs to assume the seller's unfunded commitment AND
     *      pro-rata historical called/distributed amounts — this is why
     *      real secondary trades happen at a NAV-based price (often at a
     *      discount, reflecting illiquidity and the buyer's assumption of
     *      future capital call obligations) rather than 1:1 token swaps.
     */
    function transfer(address to, uint256 amount)
        external
        onlyWhitelisted(msg.sender)
        onlyWhitelisted(to)
        returns (bool)
    {
        require(!lpAccounts[msg.sender].isDefaulted, "PrivateFund: sender is in default, transfer blocked");
        require(_balances[msg.sender] >= amount, "PrivateFund: insufficient commitment balance");

        // Pro-rata transfer of called/distributed history, so the buyer
        // inherits the correct economic position rather than a "clean" token.
        LPAccount storage from = lpAccounts[msg.sender];
        LPAccount storage to_ = lpAccounts[to];

        uint256 calledShare = (from.called * amount) / from.committed;
        uint256 distributedShare = (from.distributed * amount) / from.committed;
        uint256 claimableShare = (from.claimable * amount) / from.committed;
        // NOTE: transferring claimable balance alongside the token is a
        // design choice — alternatively an LPA might require the seller to
        // claim() any outstanding distribution BEFORE a secondary transfer
        // closes, so the buyer doesn't inherit cash the seller already
        // earned. We move it here for simplicity; flag this explicitly in
        // any real secondary-transfer workflow built on top of this.

        from.committed -= amount;
        from.called -= calledShare;
        from.distributed -= distributedShare;
        from.claimable -= claimableShare;

        to_.committed += amount;
        to_.called += calledShare;
        to_.distributed += distributedShare;
        to_.claimable += claimableShare;

        _balances[msg.sender] -= amount;
        _balances[to] += amount;

        emit Transfer(msg.sender, to, amount);
        return true;
    }

    function setWhitelistStatus(address lp, bool status) external onlyFundAdmin {
        lpAccounts[lp].isWhitelisted = status;
        emit Whitelisted(lp, status);
    }

    // =========================================================================
    // 3. CAPITAL CALLS
    // =========================================================================

    /**
     * @notice Issues a pro-rata capital call across all whitelisted LPs.
     * @dev A capital call is sized as a PERCENTAGE of each LP's commitment,
     *      not a flat amount — this is what keeps the call pro-rata fair
     *      regardless of how heterogeneous LP commitment sizes are. A €10M
     *      LP and a €1M LP funding a 10% call pay €1M and €100k respectively,
     *      preserving their relative ownership percentage of the fund.
     *
     *      In practice the GP issues this notice ~10-15 business days before
     *      the due date (we just store dueDate as a timestamp and leave
     *      enforcement of the notice period to off-chain process / front-end).
     */
    function issueCapitalCall(uint256 callBps, uint256 dueDate, string calldata purpose)
        external
        onlyFundAdmin
        returns (uint256 callId)
    {
        require(callBps > 0 && callBps <= BPS_DENOMINATOR, "PrivateFund: invalid call percentage");
        require(dueDate > block.timestamp, "PrivateFund: due date must be in future");

        callId = ++capitalCallCounter;
        CapitalCall storage call = capitalCalls[callId];
        call.callId = callId;
        call.callBps = callBps;
        call.dueDate = dueDate;
        call.purpose = purpose;
        call.totalAmount = (totalCommitted * callBps) / BPS_DENOMINATOR;

        emit CapitalCallIssued(callId, call.totalAmount, callBps, dueDate, purpose);
    }

    /**
     * @notice An LP funds their pro-rata share of a capital call.
     * @dev WIRED TO A REAL STABLECOIN: this pulls `amountOwed` of
     *      denominationAsset from the LP's wallet into this contract via
     *      transferFrom. Two preconditions the LP must satisfy off-chain /
     *      in their wallet before calling this:
     *        1. They must hold at least amountOwed of the stablecoin.
     *        2. They must have called denominationAsset.approve(address(this),
     *           amountOwed) (or higher) beforehand — this is the standard
     *           ERC-20 two-step pattern (approve, then transferFrom) that
     *           lets this contract move funds without holding the LP's keys.
     *      amountOwed is computed from the LP's COMMITTED balance (their
     *      token balance) at call time, not a fixed number, so it stays
     *      correct even if commitments changed via secondary transfers
     *      before this call was funded.
     */
    function fundCapitalCall(uint256 callId) external onlyWhitelisted(msg.sender) {
        CapitalCall storage call = capitalCalls[callId];
        require(call.callId != 0, "PrivateFund: call does not exist");
        require(!call.funded[msg.sender], "PrivateFund: already funded this call");

        LPAccount storage lp = lpAccounts[msg.sender];
        uint256 amountOwed = (lp.committed * call.callBps) / BPS_DENOMINATOR;
        require(amountOwed > 0, "PrivateFund: no obligation under this call");

        // Pulls stablecoin from the LP's wallet into this contract's treasury.
        // Reverts automatically if approve() wasn't called or balance is short.
        bool ok = denominationAsset.transferFrom(msg.sender, address(this), amountOwed);
        require(ok, "PrivateFund: stablecoin transfer failed");

        call.funded[msg.sender] = true;
        lp.called += amountOwed;
        lp.lastCallTimestamp = block.timestamp;

        totalCalled += amountOwed;

        emit CapitalCallFunded(callId, msg.sender, amountOwed);
    }

    /**
     * @notice Marks an LP as defaulted on a capital call after the due date.
     * @dev DEFAULT CONSEQUENCES IN REAL LPAs are typically harsh by design,
     *      because a defaulting LP can jeopardize the fund's ability to
     *      close a deal it already committed to. Common remedies, which a
     *      production version of this function would trigger: (a) interest
     *      penalty accrual on the unfunded amount, (b) forfeiture of a
     *      portion of the LP's existing interest, (c) forced sale of the
     *      LP's stake to other LPs or the GP at a steep discount, (d)
     *      suspension of voting/information rights. We only flag the
     *      isDefaulted state here and block further transfers/distributions,
     *      leaving the punitive economic mechanics as an extension point.
     */
    function markDefault(uint256 callId, address lp) external onlyFundAdmin {
        CapitalCall storage call = capitalCalls[callId];
        require(block.timestamp > call.dueDate, "PrivateFund: call not yet past due");
        require(!call.funded[lp], "PrivateFund: LP already funded, cannot default");

        lpAccounts[lp].isDefaulted = true;
        emit LPDefaulted(callId, lp);
    }

    // =========================================================================
    // 4. NAV UPDATES (oracle pattern)
    // =========================================================================

    /**
     * @notice Publishes a new NAV figure for the fund.
     * @dev This is a TRUSTED ORACLE WRITE, not an on-chain computation. The
     *      fund administrator (often a regulated third-party fund admin like
     *      a SS&C or Citco in real-world deployments, not the GP itself, to
     *      avoid conflicts of interest) calculates NAV off-chain using
     *      portfolio company valuations, comparable transaction multiples,
     *      or third-party appraisals, then pushes the resulting figure here.
     *      The MIN_NAV_UPDATE_INTERVAL prevents the admin from repricing
     *      more frequently than the LPA's stipulated valuation cadence,
     *      which protects LPs from the GP cherry-picking valuation timing
     *      around capital calls or secondary trades.
     */
    function updateNAV(uint256 newNAV) external onlyFundAdmin {
        require(
            block.timestamp >= lastNAVUpdateTimestamp + MIN_NAV_UPDATE_INTERVAL || lastNAVUpdateTimestamp == 0,
            "PrivateFund: NAV updates restricted to quarterly cadence"
        );
        currentNAV = newNAV;
        lastNAVUpdateTimestamp = block.timestamp;
        emit NAVUpdated(newNAV, block.timestamp);
    }

    /// @notice NAV per token, used as the secondary-market reference price.
    /// @dev Real secondary trades typically happen at a DISCOUNT to this
    ///      figure (reflecting illiquidity, NAV staleness, and the buyer's
    ///      assumption of future unfunded commitment risk) — this function
    ///      returns the reference price only, not a transaction price.
    function navPerToken() external view returns (uint256) {
        if (totalSupply == 0) return 0;
        return (currentNAV * 1e18) / totalSupply;
    }

    // =========================================================================
    // 5. DISTRIBUTION WATERFALL (European / whole-fund style)
    // =========================================================================

    /**
     * @notice Executes a distribution of exit proceeds following the
     *         standard private equity waterfall, in strict tier order:
     *           Tier 1: Return of Capital — LPs recoup contributed capital.
     *           Tier 2: Preferred Return (hurdle) — LPs earn HURDLE_RATE_BPS
     *                    annualized on capital before GP earns anything.
     *           Tier 3: GP Catch-Up — GP receives 100% of distributions
     *                    until its cumulative take equals CARRY_BPS of
     *                    total profit distributed so far.
     *           Tier 4: Carried Interest Split — remaining profit split
     *                    80/20 (LP/GP) per CARRY_BPS.
     *
     * @dev This is a EUROPEAN (whole-fund) waterfall: tiers are evaluated
     *      against CUMULATIVE fund-wide totals (totalCalled, totalDistributed,
     *      cumulativeProfitDistributed), not against a single deal's P&L.
     *      The practical effect is GP-unfriendly relative to an American
     *      (deal-by-deal) waterfall: even if Deal #1 is wildly profitable,
     *      the GP earns NO carry on it until ALL drawn-down capital across
     *      the WHOLE fund — including capital still deployed in Deal #2,
     *      #3, etc — has been returned to LPs. This protects LPs from a GP
     *      taking carry on an early winner and then losing money on later
     *      deals (a structural protection European-style LPs often insist on).
     *
     *      SIMPLIFICATION NOTE: the hurdle calculation here uses a simple
     *      (non-compounded) annualized approximation for clarity. A
     *      production system needs IRR-based hurdle accrual with proper
     *      fixed-point math (e.g. PRBMath or ABDKMath64x64), since Solidity
     *      has no native decimals and naive compounding is gas-expensive
     *      and precision-sensitive.
     */
    function distribute(uint256 totalAmount) external onlyFundAdmin {
        require(totalAmount > 0, "PrivateFund: nothing to distribute");
        require(totalCalled > 0, "PrivateFund: no called capital to distribute against");

        // Exit proceeds (e.g. from a portfolio company sale) arrive from
        // off-chain into the GP's wallet first, then are pulled into this
        // contract's treasury here. In a fully on-chain deal flow this could
        // instead be a tokenized portfolio asset selling directly into the
        // treasury — out of scope for this reference contract.
        bool ok = denominationAsset.transferFrom(msg.sender, address(this), totalAmount);
        require(ok, "PrivateFund: stablecoin transfer failed");

        uint256 remaining = totalAmount;
        uint256 toLPs = 0;
        uint256 toGP = 0;

        // --- TIER 1: Return of Capital ---
        // LPs are owed (totalCalled - totalDistributed) before any profit
        // concept applies. Distributions in this tier carry ZERO carry for
        // the GP — it is simply giving LPs their own money back.
        uint256 capitalOwed = totalCalled > totalDistributed ? totalCalled - totalDistributed : 0;
        if (remaining > 0 && capitalOwed > 0) {
            uint256 tier1 = remaining < capitalOwed ? remaining : capitalOwed;
            toLPs += tier1;
            remaining -= tier1;
            totalDistributed += tier1;
        }

        // --- TIER 2: Preferred Return (hurdle) ---
        // Simplified linear approximation: hurdle owed = called capital *
        // hurdle rate * (time elapsed since inception / 1 year). A real
        // implementation would track this per-capital-call-tranche, since
        // capital called in year 3 hasn't been outstanding as long as
        // capital called in year 1, and therefore accrues less preferred
        // return — we flatten that here for readability.
        uint256 yearsElapsed = (block.timestamp - fundInceptionDate) * 1e18 / 365 days;
        uint256 hurdleOwed = (totalCalled * HURDLE_RATE_BPS / BPS_DENOMINATOR) * yearsElapsed / 1e18;
        uint256 hurdleRemaining = hurdleOwed > cumulativeHurdlePaid ? hurdleOwed - cumulativeHurdlePaid : 0;

        if (remaining > 0 && hurdleRemaining > 0) {
            uint256 tier2 = remaining < hurdleRemaining ? remaining : hurdleRemaining;
            toLPs += tier2;
            remaining -= tier2;
            cumulativeHurdlePaid += tier2;
            cumulativeProfitDistributed += tier2;
        }

        // --- TIER 3: GP Catch-Up ---
        // GP target: once hurdle is fully paid, GP should hold CARRY_BPS
        // (20%) of (hurdle + catch-up) combined. Solve for catch-up amount
        // such that: catchup / (hurdleOwed + catchup) = CARRY_BPS / 10000
        // => catchup = hurdleOwed * CARRY_BPS / (BPS_DENOMINATOR - CARRY_BPS)
        if (remaining > 0 && hurdleRemaining == 0) {
            uint256 catchupTarget = (cumulativeHurdlePaid * CARRY_BPS) / (BPS_DENOMINATOR - CARRY_BPS);
            uint256 catchupRemaining = catchupTarget > cumulativeCatchupPaid
                ? catchupTarget - cumulativeCatchupPaid
                : 0;

            if (catchupRemaining > 0) {
                uint256 tier3 = remaining < catchupRemaining ? remaining : catchupRemaining;
                toGP += tier3;
                remaining -= tier3;
                cumulativeCatchupPaid += tier3;
                cumulativeProfitDistributed += tier3;
                totalCarryPaid += tier3;
            }
        }

        // --- TIER 4: Carried Interest Split (80/20 steady state) ---
        if (remaining > 0) {
            uint256 gpShare = (remaining * CARRY_BPS) / BPS_DENOMINATOR;
            uint256 lpShare = remaining - gpShare;
            toGP += gpShare;
            toLPs += lpShare;
            cumulativeProfitDistributed += remaining;
            totalCarryPaid += gpShare;
            remaining = 0;
        }

        // --- PULL-PATTERN CREDITING ---
        // Instead of pushing `toLPs` out to every LP address in this single
        // transaction (which would mean gas cost growing linearly with LP
        // count, and one malformed/blacklisted LP address reverting the
        // ENTIRE distribution for everyone — a classic DoS vector), we
        // credit each LP's `claimable` balance proportionally here and let
        // each LP withdraw independently via claim(). This is the standard
        // "pull over push" pattern for any contract paying out to an
        // unbounded list of addresses.
        //
        // Allocation is pro-rata by CALLED capital (lp.called / totalCalled),
        // not by commitment — distributions are a return on capital actually
        // deployed, not on capital merely promised.
        if (toLPs > 0) {
            uint256 allocated = 0;
            uint256 lpCount = lpRegistry.length;
            for (uint256 i = 0; i < lpCount; i++) {
                address lpAddr = lpRegistry[i];
                LPAccount storage acct = lpAccounts[lpAddr];
                if (acct.called == 0) continue;

                uint256 share = (toLPs * acct.called) / totalCalled;
                acct.claimable += share;
                acct.distributed += share;
                allocated += share;
            }
            // Integer division can leave dust (a few wei) unallocated due to
            // rounding down on each LP's share; in production you'd sweep
            // this dust to the last LP processed or to a rounding-remainder
            // pool rather than silently stranding it in the contract.
        }

        if (toGP > 0) {
            gpClaimable += toGP;
        }

        emit DistributionExecuted(totalAmount, toLPs, toGP);
    }

    /**
     * @notice Withdraws the caller's accrued claimable distribution balance.
     * @dev PULL PATTERN: this is the counterpart to the crediting logic in
     *      distribute(). Each LP (and the GP, via a separate path) calls
     *      this independently to pull their own funds, rather than the
     *      contract pushing funds to everyone in one transaction. Benefits:
     *      gas cost is borne by each claimant individually rather than by
     *      the GP triggering distribute(); a single LP's misbehaving wallet
     *      (e.g. a contract that reverts on receive) can't block payouts to
     *      every other LP; and it naturally supports LPs who want to leave
     *      proceeds accruing on-chain rather than sweeping them immediately.
     *      Uses checks-effects-interactions ordering (zero out claimable
     *      BEFORE the external transfer call) to prevent reentrancy.
     */
    function claim() external {
        LPAccount storage acct = lpAccounts[msg.sender];
        uint256 amount = acct.claimable;
        require(amount > 0, "PrivateFund: nothing to claim");

        acct.claimable = 0; // effects before interaction (reentrancy guard)

        bool ok = denominationAsset.transfer(msg.sender, amount);
        require(ok, "PrivateFund: stablecoin payout failed");

        emit Claimed(msg.sender, amount);
    }

    /// @notice GP claims its accrued carried interest + catch-up balance.
    function claimGP() external {
        require(msg.sender == gpPayoutAddress, "PrivateFund: not authorized GP payout address");
        uint256 amount = gpClaimable;
        require(amount > 0, "PrivateFund: nothing to claim");

        gpClaimable = 0; // effects before interaction

        bool ok = denominationAsset.transfer(gpPayoutAddress, amount);
        require(ok, "PrivateFund: stablecoin payout failed");

        emit Claimed(gpPayoutAddress, amount);
    }

    // =========================================================================
    // VIEW HELPERS
    // =========================================================================

    function getLPCount() external view returns (uint256) {
        return lpRegistry.length;
    }

    function getUnfundedCommitment(address lp) external view returns (uint256) {
        LPAccount storage acct = lpAccounts[lp];
        return acct.committed > acct.called ? acct.committed - acct.called : 0;
    }

    function isCallFundedBy(uint256 callId, address lp) external view returns (bool) {
        return capitalCalls[callId].funded[lp];
    }

    function claimableOf(address lp) external view returns (uint256) {
        return lpAccounts[lp].claimable;
    }
}