# data <- read.csv("/Users/Chinmayee/Desktop/CTCT/CTCT Income Statement.csv")

# ----------------------------------------------------------------------------
# Constants
# ----------------------------------------------------------------------------

# The time interval to suspend execution before plotting, in seconds
SLEEP_TIME_AT_START = 10


# ----------------------------------------------------------------------------
# Assumption
# ----------------------------------------------------------------------------


## Background colors
par(bg="black", fg="white", col.axis = "white",col.lab = "white" ,col.main = "white");
Quarterly.Revenue = c(
	83.494,81.256,78.874,74.931,
	72.039,70.208,68.205,66.298,
	63.846,62.072,59.938,57.532,
	54.346,52.527,50.015,47.467,
	44.828,42.455,39.481,36.455,
	33.533,30.955,28.118
);
Year = sort(c(rep(2014, 3),rep(2009:2013, 4)),decreasing = T);
qurt = c(rep(3:1),rep(4:1, 5));

REVENUE = data.frame(cbind(Year,qurt,Quarterly.Revenue));
names(REVENUE) = c("fy","Q","Rev");
REVENUE = REVENUE[order(REVENUE[, 1], REVENUE[, 2]), ];
plot(REVENUE$Rev,type = "l");

REVENUE$growth = c(
	rep(0, 4),
	REVENUE$Rev[-(1:4)] / REVENUE$Rev[-((dim(REVENUE)[1] - 3):dim(REVENUE)[1])]
	) - 1;
plot(REVENUE$growth[5:dim(REVENUE)[1]], type = "l");

growth.rate = (83.5 / 33.5) ^ (1 / 20) - 1;
REVENUE$error = REVENUE$growth - growth.rate;

# COGS
COGS = data.frame(cbind(
	2009:2013, 
	c(129.061, 174.231, 214.42, 252.154, 285.383),
	c(37.4, 49.7, 60,71.7, 79.8)
	));
names(COGS) = c("fy", "Rev", "Cost");
COGS_pct = lm(Cost~ -1 + Rev, data = COGS)$coef;
#attach(COGS);
COGS$error = COGS$Cost / COGS$Rev - COGS_pct;

# Expense
Expense = data.frame(
	cbind(2009:2013),
	c(129.061,174.231,214.42,252.154,285.383),
	c(71.5,91.6,106.2,126.8,140.4),
	c(5.1,8.6,11.7,14.3,14.7),
	c(17.2,21.5,26.3,35.1,42.2)
);

names(Expense) = c("fy", "Rev", "AdvtSMGA", "SBC", "RD");
Exp_AdvtSMGA_pct = lm(AdvtSMGA ~ -1 + Rev, data = Expense)$coef;
Exp_SBC_pct = lm(SBC ~ -1 + Rev, data = Expense)$coef; 
Exp_RD_pct = lm(RD ~ -1 + Rev, data = Expense)$coef;

attach(Expense);
Expense$AdvtSMGA_error = AdvtSMGA / Rev - Exp_AdvtSMGA_pct;
Expense$SBC_error = SBC / Rev - Exp_SBC_pct;
Expense$RD_error = RD / Rev - Exp_RD_pct;
detach(Expense);




## Forecast
Forecast <- function(M, WACC) { 
	# the total number of shares (in millions)
	numShares = 33.732;

	Year = 2014:2023;
	
	REV.forecast = data.frame(matrix(data=0, nrow = length(Year) * 4, ncol = 3));
	names(REV.forecast) = c("fy", "Q", "Rev");
	REV.forecast$fy = sort(rep(Year, 4));
	REV.forecast$Q = rep(1:4, 10);
	REV.forecast$Rev[1:3] = REVENUE[REVENUE$fy == 2014, "Rev"];
	
	error_rev = REVENUE$error[5:dim(REVENUE)[1]];
	error_COGS = COGS$error;
	error_AdvtSMGA = Expense$AdvtSMGA_error;
	error_SBC = Expense$SBC_error;
	error_RD = Expense$RD_error;
	
	Depreciation = c(25.216, 28.333, 31.414, 33.749, 35.669, 37.489, 39.395, 41,417, 43.450, 45.398);
	StockComp = c(17.137, 19.913, 23.036, 25.046, 26.554, 27.908, 29.327, 30.833, 32.346, 33.796)
	Tax_rate = c(rep(0.34, 10));
	Cap_Exp = c(23.240,27.005,31.240,33.965,36.010,37.847,39.771,41.813,43.865,45.832);
	WC = c(7.000,7.954,8.941,5.778,4.408,3.695,3.867,4.103,4.109,3.913);
	TV = 1873.29;

	discount = (1 + WACC) ^ (2014 - Year);


	aggrgt.forecast = data.frame(matrix(data = 0, nrow = length(Year), ncol = 6));
	names(aggrgt.forecast) = c("fy","Rev","COGS","Expense","EBIT", "FCF");
	aggrgt.forecast$fy = Year;

	# one column for PV, one for Price per Share, and one for each Year
	simu = data.frame(matrix(data = 0, nrow = M, ncol = 2 + length(Year)));
	names(simu) = c("PV", "Price per Share", Year);

	# to give 2 charts in one display  
	#par(mfrow = c(1,2));

	for(m in 1:M) {
		REV.forecast[4,"Rev"] = 74.931 * ( 1 + growth.rate + sample(error_rev,1) );

		for(p in 5:dim(REV.forecast)[1])
			REV.forecast[p,"Rev"] = REV.forecast[(p - 4), "Rev"] * 
			(1 + growth.rate + sample(error_rev, 1));

		aggrgt.forecast$Rev = aggregate(Rev~fy, data = REV.forecast, FUN = sum)$Rev;

		for(p in 1:length(Year)) {
			aggrgt.forecast[p, "COGS"] = aggrgt.forecast[p, "Rev"] * 
				(COGS_pct - 0.03 * (p-1) + sample(error_COGS, 1));

			aggrgt.forecast[p, "Expense"] = aggrgt.forecast[p, "Rev"] * 
				(Exp_AdvtSMGA_pct + Exp_SBC_pct + Exp_RD_pct + 
					sample(error_AdvtSMGA, 1) + sample(error_SBC, 1) + sample(error_RD, 1)
				) + 
				Depreciation[p] + StockComp[p];
			
			aggrgt.forecast[p, "EBIT"] = aggrgt.forecast[p, "Rev"] - 
				aggrgt.forecast[p, "COGS"] - aggrgt.forecast[p, "Expense"];
			
			aggrgt.forecast[p, "FCF"] = aggrgt.forecast[p, "EBIT"] * (1 - Tax_rate[p]) + 
				Depreciation[p] + StockComp[p] - Cap_Exp[p] + WC[p];
		}

		# write PV in column 1
		simu[m, 1] = sum(aggrgt.forecast$FCF * discount) + TV * discount[length(Year)];

		# write the Price per Share into column 2 (both, revenue and shares are in millions)
		simu[m, 2] = simu[m, 1] / numShares

		# write EBT for each Year
		#simu[m,2:(length(Year)+1)] = aggrgt.forecast$EBIT;

		# write revenue for each Year (first two columns are already used)
		simu[m, -(1:2)] = aggrgt.forecast$Rev;

		if (m == 1) 
			plot(Year, simu[m, -(1:2)], type = "l", col = "blue", ylab = "Revenue($mm)") 
		else
			points(Year, simu[m, -(1:2)], type = "l", col = "blue", ylab = "Revenue($mm)")
			
	}

	MeanRevPerYear <- apply(simu[ ,-(1:2)],2, mean);
	points(Year, MeanRevPerYear, type = "l", col = "orange", lwd = 5);
	# abline(v = MeanRevPerYear ,col = "orange", lwd =2);

	# main = "Price per share"
  hist(simu[, 2], col = "blue",xlab = "Price per Share", main = "Price per share");
	x <- mean(simu[,2]);
	abline(v = 38.6,col = "orange", lwd =5);
	abline(v = x,col = "green", lwd =5);
	box();

	print("PV summary:");
	print(summary(simu[, "PV"]));

	print("Price per Share:");
	print(summary(simu[, "Price per Share"]));

	print("Revenue for each Year");
	print(summary(simu[, -(1:2)]));

	print("5th percentile for each column:")
	for (col in 1:dim(simu)[2]) {
		print(names(simu)[col]);
		print(quantile (simu[, col], 0.05));
		}
		
	print("95th percentile for each column:")
	for (col in 1:dim(simu)[2]) {
		print(names(simu)[col]);
		print(quantile (simu[, col], 0.95));
	}
	
	return(simu);
}

# stops execution for given time to have a chance to start screen recording
Sys.sleep(SLEEP_TIME_AT_START)

try = Forecast(1000, 0.103);