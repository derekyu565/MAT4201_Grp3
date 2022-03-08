import math
import matplotlib.pyplot as plt

f = lambda x: math.e**x + x - 6           #x**4 - x**2 #lambda allows for us to create 1-line functions
						# double asterisks (**) is for exponents
#f = lambda x: x**2 + (x / x+1) - 3 

def bisection(f,a,b,N):
    if f(a)*f(b) >= 0:
        print("Bisection method fails. We already have a solution")
        if f(a)== 0:
            print("f("+str(a)+") equates to 0")
        elif f(b)==0:
            print("f("+str(b)+") equates to 0")
        return None
    
    a_n = a
    b_n = b
    
    for n in range(1,N+1):
        m_n = (a_n + b_n)/2
        f_m_n = f(m_n)
        if f(a_n)*f_m_n < 0:
            a_n = a_n
            b_n = m_n
        elif f(b_n)*f_m_n < 0:
            a_n = m_n
            b_n = b_n
        elif f_m_n == 0:
            print("Found exact solution. At iteration; ",n)
            return m_n
        else:
            print("Bisection method fails.")
            return None
        
    return (a_n + b_n)/2

#plt.plot(f)
#plt.show()

print(bisection(f, 1, 2, 100))