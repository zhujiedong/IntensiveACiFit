#' Estimate photosynthesis parameters for C4 species using Sharkey's fitting procedure
#'
#' Using the gas exchange measurement (A_Ci curve), C4 photosynthesis model without
#' carbonic anhydrase and Sharkey et al. (2007) fitting processure to do nonlinear
#' curve fitting (using nlminb package) for estimating photosynthesis parameters
#' (Vcmax,J,Rd,gm and Vpmax) for C4 species. The difference
#' between this method with C4EstimateWithoutCA is that temperature response
#' parameters need to be provided by the users. Thus, this method provides the option
#' to alter temperature response parameters. If only planing to alter several
#' parameters, not all of them, one can use the other parameters provided by
#' Table S1 in Zhou et al. (2019) ("Deriving C4 photosynthesis parameters by fitting
#' intensive A/Ci curves"). Make sure to load the "stats" package
#' before intstalling and using the "C4Estimation" package.
#' @param ACi     Gas exchange measurement from Li6400 or other equipment. It is a
#' dataframe iput. Ci with the unit of ppm. You can prepare the data in Excel file
#' like the given example and save it as "tab delimited text". Then import data by
#' ACi = read.table(file = "/Users/haoranzhou/Desktop/Learn R/ACi curve.txt",header
#' = TRUE)
#' @param alpha1  The fraction of O2 evolution occurring in the bundle sheath. Unless
#' you have enough information, input it as the 0.15.
#' @param x       the fraction of total electron transport that are confined to be
#' used for the PEP regeneration out of J, which is the total electron transport.
#' @param CaBreakL  Higher bound of Ci below which A is thought to be controled by
#' Rubisco Carboxylation (Ac). Start with 10.
#' @param CaBreakH  Lower bound of Ci above which A is thought to be controled by RuBP
#' regeneration (Aj). Start with 50. If the estimation results showed "inadmissible
#' fits", change the CaBreakL and CaBreakH until "inadmissible fits" disappear.
#' @param startp    A vector that gives the start points for the estimation (c(Vcmax,
#' J,Rd,gm and Vpmax))
#' @param TresponseKc A vector that gives the temperature response parameters for the
#' Kc (c(Kc25,deltaHaKc))
#' @param TresponseKo A vector that gives the temperature response parameters for the
#' Ko (c(Ko25,deltaHaKo))
#' @param Tresponsegammastar A vector that gives the temperature response parameters for the
#' gammastar (c(gammastar25,deltaHagammastar))
#' @param TresponseKp A vector that gives the temperature response parameters for the
#' Kp (c(Kp25,deltaHaKp))
#' @param Tresponsegbs A vector that gives the temperature response parameters for the
#' gbs (c(gbs25,deltaHagbs,deltaHdgbs,deltaSgbs))
#' @param TresponseVcmax A vector that gives the temperature response parameters for the
#' Vcmax (c(deltaHaVcmax))
#' @param TresponseJ A vector that gives the temperature response parameters for the
#' J (c(deltaHaJ,deltaHdJ,deltaSJ))
#' @param TresponseRd A vector that gives the temperature response parameters for the
#' Rd (c(deltaHaRd))
#' @param Tresponsegm A vector that gives the temperature response parameters for the
#' gm (c(deltaHagm,deltaHdgm,deltaSgm))
#' @param TresponseVpmax A vector that gives the temperature response parameters for the
#' Vpmax (c(deltaHaVpmax,deltaHdVpmax,deltaSVpmax))
#' @return This package will return a dataframe that contains the following values
#' (c(Vcmax,J,Rd,gm and Vpmax)). You can try with c(30, 150, 3, 10, 50).
#' @return Parameter at leaf temperature:      A vector (c(Vcmax,J,Rd,gm and Vpmax))
#' returns the estimation parameters at leaf temperature.
#' @return Parameter at 25°C:                  A vector (c(Vcmax,J,Rd,gm and Vpmax))
#' returns the estimation parameters at leaf temperature.
#' @return Objective:                          The final objective value based on the
#' estimation results.
#' @return Convergence:                        An integer code. 0 indicates successful
#' convergence.
#' @return Message:	                          A character string giving any additional
#' information returned by the optimizer, or NULL. For details, see PORT documentation.
#' @return Iterations:	                        Number of iterations performed.
#' @return Evaluations:	                      Number of objective function and gradient
#' function evaluations.
#' @export


C4EstimateWithoutCAT = function(
    ACi, alpha1 = 0.15, x = 0.4, CaBreakL = 10, CaBreakH = 45,
    startp = c(45, 300, 3, 20, 120),
    TresponseKc = c(75.06, 36.5), TresponseKo = c(35.82, 12.8),
    Tresponsegammastar = c(0.000244, 24.82),
    TresponseKp = c(8.55, 52.2), 
    Tresponsegbs = c(0.02946, 116.77, 264.6, 0.86),
    TresponseVcmax = c(51.89), 
    TresponseJ = c(69.25, 188.502, 0.609),
    TresponseRd = c(41.85), Tresponsegm = c(46.533, 366.8, 1.2),
    TresponseVpmax = c(65.69, 147.694, 0.47),
    TresponseKp2 = c(33.54,27.2),
    Tresponsekr = c(0.003330, 65.2704), Tresponsekf = c(0.038986, 74.8936),
    TresponseKH = c(2.9799)
    ){

    A.obs = ACi$A
    Ci.obs = ACi$Pci
    Patm = mean(ACi$Pa, na.rm = TRUE)
    Tleaf = mean(ACi$TleafCnd, na.rm = TRUE)
    O2 = Patm*0.21

    Kc25 = TresponseKc[1]
    deltaHaKc = TresponseKc[2]
    Ko25 = TresponseKo[1]
    deltaHaKo = TresponseKo[2]
    gammastar25 = Tresponsegammastar[1]
    deltaHagammastar = Tresponsegammastar[2]
    Kp25 = TresponseKp[1]
    deltaHaKp = TresponseKp[2]
    gbs25 = Tresponsegbs[1]
    deltaHagbs = Tresponsegbs[2]
    deltaHdgbs = Tresponsegbs[3]
    deltaSgbs = Tresponsegbs[4]
    deltaHaVcmax = TresponseVcmax[1]
    deltaHaJ = TresponseJ[1]
    deltaHdJ = TresponseJ[2]
    deltaSJ = TresponseJ[3]
    deltaHaRd = TresponseRd[1]
    deltaHagm = Tresponsegm[1]
    deltaHdgm = Tresponsegm[2]
    deltaSgm = Tresponsegm[3]
    deltaHaVpmax = TresponseVpmax[1]
    deltaHdVpmax = TresponseVpmax[2]
    deltaSVpmax = TresponseVpmax[3]

    # Temperature adjustment for Kc,Ko,gammastar,Kp from 25°C to Tleaf
    Kc=Kc25*exp(deltaHaKc*(Tleaf-25)/(298.15*0.008314*
        (273.15+Tleaf)))
    Ko=Ko25*exp(deltaHaKo*(Tleaf-25)/(298.15*0.008314*
        (273.15+Tleaf)))
    gammastar=gammastar25*exp(deltaHagammastar*(Tleaf-25)/
        (298.15*0.008314*(273.15+Tleaf)))
    Kp=Kp25*exp(deltaHaKp*(Tleaf-25)/(298.15*0.008314*(273.15+
        Tleaf)))
    gbs=gbs25*exp(deltaHagbs*(Tleaf-25)/(298.15*0.008314*
        (273.15+Tleaf)))*(1+exp((298.15*deltaSgbs-deltaHdgbs)/
        298.15/0.008314))/(1+exp(((273.15+Tleaf)*deltaSgbs-
        deltaHdgbs)/(273.15+Tleaf)/0.008314))

    # Define objective function
    fn=function(Param){
        Vcmax = Param[1]
        J = Param[2]
        Rd = Param[3]
        gm = Param[4]
        Vpmax = Param[5]
        Rm = Rd/2

        #Useful intermediates
        Obs = alpha1*(A.obs+Rd)/(0.047*gbs)/1000+O2
        Vpc = Vpmax*(gm*Ci.obs-A.obs)/(gm*Ci.obs-A.obs+Kp*gm)
        Vpr = x*J/2
        Cbspc = Vpmax*(gm*Ci.obs-A.obs)/(gm*Ci.obs-A.obs+Kp*
            gm)/gbs-Rd/2/gbs-A.obs/gbs+Ci.obs-A.obs/gm
        Cbspr = x*J/2/gbs-Rd/2/gbs-A.obs/gbs+Ci.obs-A.obs/gm
        Acpc = Vcmax*(Cbspc-gammastar*Obs*1000)/(Cbspc+Kc*
            (1+Obs/Ko))
        Acpr = Vcmax*(Cbspr-gammastar*Obs*1000)/(Cbspr+Kc*
            (1+Obs/Ko))
        Ajpc = (1-x)*J*(Cbspc-gammastar*Obs*1000)/(4*Cbspc+
            8*gammastar*Obs*1000)
        Ajpr = (1-x)*J*(Cbspr-gammastar*Obs*1000)/(4*Cbspr+8*
            gammastar*Obs*1000)

        #Objective
        sum(((Ci.obs<=CaBreakL)*(Acpc-Rd)+
            (Ci.obs>CaBreakL)*(Ci.obs<CaBreakH)*
            ((Vpc<Vpr)*((Acpc<Ajpc)*Acpc+(Acpc>=Ajpc)*Ajpc)+
            (Vpc>=Vpr)*((Acpr<Ajpr)*Acpr+(Acpr>=Ajpr)*Ajpr)-Rd)+
            (Ci.obs>=CaBreakH)*(Ajpr-Rd)-A.obs)^2)
        }

  # Using constrained optimization package "nloptr" to estimate Vcmax,J,Rd,gm and Vpmax
    Est.model = nlminb(c(startp[1],startp[2] , startp[3], startp[4], 
        startp[5]), fn, lower=c(0,0,0,0,0), 
        upper=c(200, 600, 20, 30, 200))

    Parameters=Est.model$par
    Vcmax = Parameters[1]
    J = Parameters[2]
    Rd = Parameters[3]
    gm = Parameters[4]
    Vpmax = Parameters[5]
    Rm = Rd/2

    #Temperature adjustment for Vcmax,J,Rd,gm and Vpmax from Tleaf to 25°C
    Vcmax25=Parameters[1]/(exp(deltaHaVcmax*(Tleaf-25)/(298.15*0.008314*(273.15+Tleaf))))
    J25=Parameters[2]/(exp(deltaHaJ*(Tleaf-25)/(298.15*0.008314*(273.15+Tleaf)))*
                        (1+exp((298.15*deltaSJ-deltaHdJ)/298.15/0.008314))/
                        (1+exp(((273.15+Tleaf)*deltaSJ-deltaHdJ)/(273.15+Tleaf)/0.008314)))
    Rd25=Parameters[3]/(exp(deltaHaRd*(Tleaf-25)/(298.15*0.008314*(273.15+Tleaf))))
    gm25=Parameters[4]/(exp(deltaHagm*(Tleaf-25)/(298.15*0.008314*(273.15+Tleaf)))*
                        (1+exp((298.15*deltaSgm-deltaHdgm)/298.15/0.008314))/
                        (1+exp(((273.15+Tleaf)*deltaSgm-deltaHdgm)/(273.15+Tleaf)/0.008314)))
    Vpmax25=Parameters[5]/(exp(deltaHaVpmax*(Tleaf-25)/(298.15*0.008314*(273.15+Tleaf)))*
                            (1+exp((298.15*deltaSVpmax-deltaHdVpmax)/298.15/0.008314))/
                            (1+exp(((273.15+Tleaf)*deltaSVpmax-deltaHdVpmax)/(273.15+Tleaf)/
                                    0.008314)))

    para25 = data.frame(Vcmax25, J25, Rd25, gm25, Vpmax25)
    para = data.frame(Vcmax, J, Rd, gm, Vpmax)
    # Calculate the estimation results

    # Useful intermediate
    O2=Patm*0.21*1000
    x1_ac = Vcmax
    x2_ac = Kc/Ko/1000
    x3_ac = Kc
    deno_ac = gm+gbs-x2_ac*gm*alpha1/0.047

    x1_aj = (1-x)*J/4
    x2_aj = 2*gammastar
    x3_aj = 0
    deno_aj = gm+gbs-x2_aj*gm*alpha1/0.047

    # Explicit calculation of AEE and AET
    d = gm*(Rm-Vpmax-Ci.obs*(gm+2*gbs)-Kp*(gm + gbs))
    f = gm*gm*(Ci.obs*Vpmax+(Ci.obs+Kp)*(gbs*Ci.obs-Rm))
    k = gm*gm*gbs*(Ci.obs+Kp)

    RcPc_p = (d-(x3_ac+x2_ac*O2)*gm*gbs+(Rd-x1_ac)*(gm+gbs)-
                (x1_ac*gammastar*gm+x2_ac*Rd*gm-x2_ac*k/gbs)*alpha1/0.047)/deno_ac
    RrPc_p = (d-(x3_aj+x2_aj*O2)*gm*gbs+(Rd-x1_aj)*(gm+gbs)-
                (x1_aj*gammastar*gm+x2_aj*Rd*gm-x2_aj*k/gbs)*alpha1/0.047)/deno_aj
    RcPc_q = (f+(x3_ac+x2_ac*O2)*k+d*(Rd-x1_ac)-gm*gbs*
                (x1_ac*gammastar*O2+Rd*(x3_ac+x2_ac*O2))+
                (x1_ac*gammastar+x2_ac*Rd)*k*alpha1/0.047/gbs)/deno_ac
    RrPc_q = (f+(x3_aj+x2_aj*O2)*k+d*(Rd-x1_aj)-gm*gbs*
                (x1_aj*gammastar*O2+Rd*(x3_aj+x2_aj*O2))+
                (x1_aj*gammastar+x2_aj*Rd)*k*alpha1/0.047/gbs)/deno_aj
    RcPc_r = (Rd*(f+(x3_ac+x2_ac*O2)*k)-x1_ac*(f-k*gammastar*O2))/deno_ac
    RrPc_r = (Rd*(f+(x3_aj+x2_aj*O2)*k)-x1_aj*(f-k*gammastar*O2))/deno_aj

    RcPc_Q = (RcPc_p*RcPc_p-3*RcPc_q)/9
    RrPc_Q = (RrPc_p*RrPc_p-3*RrPc_q)/9
    RcPc_U = (2*RcPc_p^3-9*RcPc_p*RcPc_q+27*RcPc_r)/54
    RrPc_U = (2*RrPc_p^3-9*RrPc_p*RrPc_q+27*RrPc_r)/54
    RcPc_PHI = acos(RcPc_U/(RcPc_Q^3)^0.5)
    RrPc_PHI = acos(RrPc_U/(RrPc_Q^3)^0.5)

    RcPc = -2*RcPc_Q^0.5*cos(RcPc_PHI/3)-RcPc_p/3
    RrPc = -2*RrPc_Q^0.5*cos(RrPc_PHI/3)-RrPc_p/3


    # Explicit calculation of ATE and ATT
    Vpr = x*J/2

    a_ac = x2_ac*gm*alpha1/0.047-gm-gbs
    a_aj = x2_aj*gm*alpha1/0.047-gm-gbs
    b_ac = gm*(Ci.obs*gbs+Vpr-Rm)+(x3_ac+x2_ac*O2)*gm*gbs+
            (x1_ac*gammastar+x2_ac*Rd)*gm*alpha1/0.047+(gm+gbs)*(x1_ac-Rd)
    b_aj = gm*(Ci.obs*gbs+Vpr-Rm)+(x3_aj+x2_aj*O2)*gm*gbs+
            (x1_aj*gammastar+x2_aj*Rd)*gm*alpha1/0.047+(gm+gbs)*(x1_aj-Rd)
    c_ac = -gm*(Ci.obs*gbs+Vpr-Rm)*(x1_ac-Rd)+gm*gbs*
            (x1_ac*gammastar*O2+Rd*(x3_ac+x2_ac*O2))
    c_aj = -gm*(Ci.obs*gbs+Vpr-Rm)*(x1_aj-Rd)+gm*gbs*
            (x1_aj*gammastar*O2+Rd*(x3_aj+x2_aj*O2))

    RcPr = (-b_ac+(b_ac^2-4*a_ac*c_ac)^0.5)/2/a_ac
    RrPr = (-b_aj+(b_aj^2-4*a_aj*c_aj)^0.5)/2/a_aj

    Vpc_RcPc = Vpmax*(Ci.obs-RcPc/gm)/((Ci.obs-RcPc/gm)+Kp)
    Vpc_RrPc = Vpmax*(Ci.obs-RrPc/gm)/((Ci.obs-RrPc/gm)+Kp)

    Ac = (Vpc_RcPc<=Vpr)*RcPc+(Vpc_RcPc>Vpr)*RcPr
    Aj = (Vpc_RrPc<=Vpr)*RrPc+(Vpc_RrPc>Vpr)*RrPr

    # Calculate the real estimation error term

    Error1 = sum((A.obs-(Ac<=Aj)*Ac-(Ac>Aj)*Aj)^2)

    #Write a while loop to compare Ac and Aj
    Count=length(Ci.obs)
    limitation = rep(0,Count)
    i = 1
    while (i<=Count){
        limitation[i] = 2*(Ac[i]>=Aj[i])+1*(Ac[i]<Aj[i])
        i=i+1
    }

    # Print out to see whether there is inadmissible fit
    Ci_name  =  "Ci"
    limitation_name  =  "limitation_state"
    inadmissible_fit  =  data.frame(Ci.obs,limitation)
    colnames(inadmissible_fit) = c(Ci_name,limitation_name)

    # Return the estimation results
    modle_optimization = list(objective = Est.model$objective,
        convergence = Est.model$convergence,
        message = Est.model$message,
        iterations = Est.model$iterations,
        evaluations = Est.model$evaluations
        )
    limitation_stage = list(RcPc = RcPc, RcPr = RcPr, 
        RrPc = RrPc, RrPr = RrPr)
    results =  list(modle_optimization, 
    par = para, par25 = para25,
    limitation_stage = limitation_stage,
    inadmissible_fit = inadmissible_fit
    )
    return(results)
}
