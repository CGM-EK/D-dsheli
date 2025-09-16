#Backward selection

#Test af lineære modeller

Lm.test.5spg <- lm(dfalt$pfv~
                     dfalt$fam.sit.bag+
                     dfalt$fam.sit.frem+
                     dfalt$dk.sit.bag+
                     dfalt$dk.sit.frem+
                     dfalt$an.str.fbg)
summary(Lm.test.5spg)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         3.79347    1.01857   3.724 0.000331 ***
#  dfalt$fam.sit.bag   0.10066    0.07725   1.303 0.195686    
#dfalt$fam.sit.frem -0.14923    0.10422  -1.432 0.155427    
#dfalt$dk.sit.bag    0.06635    0.01678   3.955 0.000147 ***
#  dfalt$dk.sit.frem  -0.02146    0.03157  -0.680 0.498252    
#dfalt$an.str.fbg    0.08108    0.04207   1.927 0.056935 .  

#her anvendes backward selection

#variablen højest p-værdi fjernes dk.sit.frem
lm.test.4spg <- lm(dfalt$pfv~
                     dfalt$fam.sit.bag+
                     dfalt$fam.sit.frem+
                     dfalt$dk.sit.bag+
                     dfalt$an.str.fbg)
summary(lm.test.4spg)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         4.03559    0.95163   4.241 5.09e-05 ***
#  dfalt$fam.sit.bag   0.13490    0.05841   2.309   0.0230 *  
#  dfalt$fam.sit.frem -0.19630    0.07768  -2.527   0.0131 *  
#  dfalt$dk.sit.bag    0.06137    0.01505   4.077 9.35e-05 ***
#  dfalt$an.str.fbg    0.06844    0.03764   1.818   0.0721 .  

#an.str.fbg fjernes pga højeste p-værdi
lm.test.3spg <- lm(dfalt$pfv~
                     dfalt$fam.sit.bag+
                     dfalt$fam.sit.frem+
                     dfalt$dk.sit.bag)
summary(lm.test.3spg)
#alle variabler har nu statistisk signifikans                   
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         3.02933    0.78323   3.868 0.000198 ***
#  dfalt$fam.sit.bag   0.17231    0.05531   3.116 0.002408 ** 
#  dfalt$fam.sit.frem -0.17793    0.07792  -2.284 0.024558 *  
#  dfalt$dk.sit.bag    0.06389    0.01517   4.213 5.61e-05 ***

#Da vi nu har 3 variabler der alle har signifikans for modellen må vi konkludere
# at ud fra backwards selection er det her den optimale MLR