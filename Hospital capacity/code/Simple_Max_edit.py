
import numpy as np
from scipy.integrate import odeint
import pandas as pd



def model_full(N,InitCaseNo,R0,NoWeeksPred):

    # SEIR Model

    def model_Disease(z, t, beta, gamma, N):
        S = z[0]
        E = z[1]
        I = z[2]
        R = z[3]

        dSdt = -beta * I*S/N
        dEdt = beta * I*S/N - alpha*E
        dIdt = alpha*E - gamma*I

        dIndt = alpha*E
        dRdt = gamma * I
        return [dSdt, dEdt, dIdt, dRdt, dIndt]
    
    # time points for evaluation
    t = range(0,NoWeeksPred*7,1)

    # solve ODEs
    y0 = [N,0,InitCaseNo,0,InitCaseNo]
    df = pd.DataFrame()

    for i in range(500):

        alpha = np.random.uniform(0.181818, 0.28571)
        gamma = np.random.uniform(0.133333, 0.28571)
        beta = gamma*R0

        y1 = odeint(model_Disease, y0, t, args=(beta, gamma, N))
        

        # store info in the dataframe
        df[str(i) + "I"] = np.array(y1[:, 2])



    # Plotting the baseline (red) curve
    alpha = 1./5
    gamma = 1./5
    beta = gamma*R0
    y1 = odeint(model_Disease, y0, t, args=(beta, gamma, N))
    


    # store info in the dataframe
    df["BaseI"] = np.array(y1[:, 2])
    return df


