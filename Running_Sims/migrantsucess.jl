#Code to run simplified simulations looking at migrant mating sucess (data for figure S9)
#load up needed pacakges
using Distributions
using Plots
using CSV
using DataFrames

#make vectors of populations males and females separate N = 2000
#migrants are 1, nonmigrants are 2
#females =cat(fill(1,10),fill(0,990),dims=1)
#males =cat(fill(1,10),fill(0,990),dims=1)
#make vectors of populations males and females separate N = 10,000
#if you want to run small population size comment these females and males lines and uncomment the ones above.
females =cat(fill(1,50),fill(0,4950),dims=1)
males =cat(fill(1,50),fill(0,4950),dims=1)

#the risk/intensity of sperm competition
rs=[0,0.25,0.5,0.75,2,3,4,5,6,7,8,9,10]
rs=0.25
#declaring the vector that we will fill with 
results=fill(0,(10000,13))
for rep in 1:10000
    success=fill(0,(5000,13))#change 5000 to 1000 for small populations
    #count of row so number of males
    count=1
    #count of column (risk/intesnity of of sperm competition)
    countr=1
    for r in rs
        count=1
        for i in females
            if r<1#risk of sperm competition
                if wsample([true,false],[r,1-r],1)[1]#if true need to randomly sample the males
                    success[count,countr]=sum(sample(males,2)*i)#only will be one if female (i) is also one aka a migrant
                else
                    success[count,countr]=sample(males,1)[1]#no sperm competiton migrant male will have success regardless

                end
                count=count+1
            else#intensity of sperm competition
                success[count,countr]=sum(sample(males,Int(r))*i)#sample r males and multiple it by i. only will be one if female (i) is also one aka a migrant
                count=count+1
            end
        end
        countr=countr+1
    end
    success.=min.(success,1)#only care about if there was a migrant male sucess, so doesn't matter if a migrant female mates with more than one migrant
   results[rep,:] = sum(success,dims=1)#sum how many successful mating there were across all individuals
end
results
#make results as a data frame
ResultsDF=DataFrame(results,Symbol.(string.(rs)))

#save simulation results
CSV.write("Simulation_migrantmating.csv",ResultsDF)