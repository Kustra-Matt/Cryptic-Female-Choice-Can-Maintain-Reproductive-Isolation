#Code to run the model for "Cryptic female choice can maintain reporductive isolation"
#Currently set to run for no Ecological divergence but there are cyrptic preference divergence (Scenario 1) and at large populations size (10,000)
#intensities of sperm compeitition

#load up package for distributing work on many cpu's
using Distributed
#add the number of procesess i.e. cores being used
addprocs(23)

#load up packages across all cores
#the @everywhere tag executes the code across all cores
@everywhere using Random, Distributions, StatsBase, GLM, DataFrames,CSV,SharedArrays

#mutation distribution of alleles for 20 Loci runs, comment out next line to run 2 loci
#@everywhere const MTD=Normal(0,(0.25^2/40)^0.5)

#Function to count occurances
@everywhere function countmemb(itr,N)
    d=fill(0,N)
    for i in itr
        d[i] += 1
    end
    return d
end

#Function to count occurances for offsrping of males belonging to mothers
@everywhere function countmemb2(itr,fit,N,migrant)
    d=fill(0,N)
    count=1
    for i in itr
        if count in migrant
        else
            d[i] += fit[count]
        end
        count+=1
    end
    return d
end

#Function to count number of males that survived per mother
@everywhere function countmemb3(itr,fit,N,surv,migrant)
    d=fill(0,N)
    count=1
    for i in itr
        if count in migrant
        else
            if count in surv
            d[i] += 1
            else
            end
        end
        count+=1
    end
    return d
end

#constant array for whether a mutation occurs
#@everywhere const mutats=[0.005,1-0.005]
@everywhere const tf=[true,false]
#probability of mating sucess eq.1 in main text
@everywhere function mate(x,a,b)
  1-(1/(1+exp(-a*(x-b))))
end

#generate population function
@everywhere  function start_geno(TD1,TD2,TD3,TD4,n,l,pop)
    #####initialization of simulations
    #make maternal genome females
    mgf::Array{Float64,3}=cat(rand(TD1,(n,l)),rand(TD2,(n,l)),rand(TD3,(n,l)),rand(TD4,(n,l)),fill(pop,(n,l)),dims=3)
  
    #convert negative genotypic values to 0
    #mgf[mgf.< 0] .= 0
  
    #make paternal genome females
    pgf::Array{Float64,3}=cat(rand(TD1,(n,l)),rand(TD2,(n,l)),rand(TD3,(n,l)),rand(TD4,(n,l)),fill(pop,(n,l)),dims=3)
  
    #convert negative genotypic values to 0
    #pgf[pgf.< 0] .= 0
  
    #make maternal genome males
    mgm::Array{Float64,3}=cat(rand(TD1,(n,l)),rand(TD2,(n,l)),rand(TD3,(n,l)),rand(TD4,(n,l)),fill(pop,(n,l)),dims=3)
  
    #convert negative genotypic values to 0
    #mgm[mgm.< 0] .= 0
  
    #make paternal genome males
    pgm::Array{Float64,3}=cat(rand(TD1,(n,l)),rand(TD2,(n,l)),rand(TD3,(n,l)),rand(TD4,(n,l)),fill(pop,(n,l)),dims=3)
  
    #convert negative genotypic values to 0
   #pgm[pgm.< 0] .= 0
  
    return(mgf,pgf,mgm,pgm)
end

#Probability of fertilization sucess function (eq.9 in text)
# '.' makes function vectorized
@everywhere @views function prob_success(males,a,d,matesM)
    malesT=males[matesM,2]
    malesS=males[matesM,3]
    prob=exp.((.-(malesT .- d).^2)./(2 .*a)) .* (malesS)
    #print(prob)
    #check if any is zero or NAN
    if !any(isnan.(prob)) & !all(prob.==0)
      return(prob./sum(prob))
    #If both are NA or zero just use sperm number
    elseif all(isnan.(prob)) | all(prob.==0)
      return(malesS./sum(malesS))
    else
      #if only one is zero or NA then just use the closest match.
      if abs(malesT[1]-d)>abs(malesT[2]-d)
          return([1,0])
      else
          return([0,1])
      end
      end
  end

#survival function for ecological divergence
#for small population set to 500 instead of 2500
@everywhere @views function prob_surv!(x,a,d)
    #calculate probability
  prob=1 .- exp.((.-(x[:,4] .- d).^2)/(2 .*a))
  prob[isnan.(prob)].=1
  dead=wsample(1:1:size(x)[1],prob,2500,replace=false)
  x[dead,:]=fill(NaN,(2500,5))
end

@everywhere function mutate(gene)
    @views if wsample(tf,mutats,1)[1]#seeing if mutation happens
      mut=(rand(MTD,1))[1] #if mutation happens draw from mutation distribution
      gene=gene.+mut #add mutational effect
    else #if no mutation occurs just return same gene value
      return(gene)
    end
end
#Function to make a gamete, i.e.
#sample a single allele per loci randomly from paternal and maternal copies
#pg = paternal genome, mg = maternal genome
#ind = index/ID of dad or mom

@everywhere @views function make_gamete(pg,mg,ind)
    #initialization of new gamete
    gamete=zeros(1,size(pg)[2],5)
    @views for j in 1:size(pg)[2] #iterate through loci
        for k in 1:5 #iterate through traits
            if sample(tf) #sample true or false
                gamete[1,j,k]=pg[ind,j,k] #if true take from paternal
            else
                gamete[1,j,k]=mg[ind,j,k] #if false take from maternal
            end
        end
    end
  return gamete #return gamate
end



#simulation function
#N = population size
#mgfe = maternal genome of females in the population
#pgfe = paternal geneome of females in the population
#mgma= maternal genome of males in the population
#pgma = paternal genome of males in the population
#a = strength of selection (lower is stronger selection)
#rsc = risk/intensity of sperm competition, probability a female mates with a male.
#Ranges 0 to 3. e.g. 2.5 means mates with 2 males and 50% probability a third male.
#tradeoff= indicates whether a tradeoff between sperm number and sperm male trait exists
#generations = how many generations
#d is optimal sperm trait value. For cryptic female choice this is ignored.
#2loci
#m is migration
#an is strength of natural selection
@everywhere @views function sim(N,m,an,a,rsc,tradeoff,generations,d=-1)
   #Constants for run
    #carrying capacity 
    #for small population set to 500
    K = 2500
    #Population A starting sperm/ejaculate investment
    #starting keys for sperm number for given scenarios.
    #these numbers are the averages from initial simulations once they reached equalibrium
    ast=[1	10	0	80.46048
    1	3	0	32.56294
    1	4	0	28.47598
    1	5	0	25.72258
    1	6	0	24.62201
    1	7	0	45.44101
    1	8	0	58.69654
    1	9	0	68.03499
    12.5	10	0	87.89251
    12.5	3	0	39.85161
    12.5	4	0	44.18629
    12.5	5	0	48.26471
    12.5	6	0	52.16314
    12.5	7	0	57.18978
    12.5	8	0	63.56473
    12.5	9	0	72.47898
    50	10	0	98.10464
    50	3	0	40.99098
    50	4	0	45.53772
    50	5	0	49.67005
    50	6	0	54.23125
    50	7	0	59.46413
    50	8	0	66.50148
    50	9	0	77.4446
    1	10	1	31.8839
    1	3	1	40.98159
    1	4	1	42.26435
    1	5	1	29.95808
    1	6	1	25.2214
    1	7	1	24.81026
    1	8	1	27.27168
    1	9	1	31.64317
    12.5	10	1	107.94115
    12.5	3	1	50.01274
    12.5	4	1	55.19001
    12.5	5	1	60.12489
    12.5	6	1	65.067
    12.5	7	1	71.32932
    12.5	8	1	79.0377
    12.5	9	1	88.33272
    50	10	1	119.72179
    50	3	1	51.33736
    50	4	1	56.80187
    50	5	1	61.97386
    50	6	1	67.55966
    50	7	1	74.17665
    50	8	1	83.0212
    50	9	1	96.68915]
    #Population B starting sperm/ejaculate investment
    #starting keys for sperm number for given scenarios.
    #these numbers are the averages from initial simulations once they reached equalibrium
    bst=[1	10	0	80.46048
    1	3	0	32.56294
    1	4	0	28.47598
    1	5	0	25.72258
    1	6	0	24.62201
    1	7	0	45.44101
    1	8	0	58.69654
    1	9	0	68.03499
    12.5	10	0	87.89251
    12.5	3	0	39.85161
    12.5	4	0	44.18629
    12.5	5	0	48.26471
    12.5	6	0	52.16314
    12.5	7	0	57.18978
    12.5	8	0	63.56473
    12.5	9	0	72.47898
    50	10	0	98.10464
    50	3	0	40.99098
    50	4	0	45.53772
    50	5	0	49.67005
    50	6	0	54.23125
    50	7	0	59.46413
    50	8	0	66.50148
    50	9	0	77.4446
    1	10	1	15.94195
    1	3	1	20.4908
    1	4	1	21.13218
    1	5	1	14.97904
    1	6	1	12.6107
    1	7	1	12.40513
    1	8	1	13.63584
    1	9	1	15.82158
    12.5	10	1	53.97057
    12.5	3	1	25.00637
    12.5	4	1	27.59501
    12.5	5	1	30.06245
    12.5	6	1	32.5335
    12.5	7	1	35.66466
    12.5	8	1	39.51885
    12.5	9	1	44.16636
    50	10	1	59.86089
    50	3	1	25.66868
    50	4	1	28.40093
    50	5	1	30.98693
    50	6	1	33.77983
    50	7	1	37.08833
    50	8	1	41.5106
    50	9	1	48.34457]
    ####Initial trait distributions for population A
    #Sperm number average for Population A taken from the above key
    muS_a=ast[ast[:,1] .== a .&& ast[:,2] .==rsc .&& ast[:,3].==tradeoff  ,4]

    ##For no divergence in sexually selected traits make sure td1_a = td1_b,  td2_a = td2_b,  td3_a = td3_b (Scenario 2)
    ##Female
    td1_a = Normal(40/40,(2.5^2/40)^0.5)
    ##Male
    td2_a = Normal(40/40,(2.5^2/40)^0.5)
    ##sperm Count
    td3_a = Normal(muS_a[1]/40,(2.5^2/40)^0.5)
    ##Ecological trait
    td4_a = Normal(40/40,(2.5^2/40)^0.5)
    ###actually generating the genomes
    #mgf_a is maternal genome for females in population a
    #pgf_a is paternal genome for females in population a
    #mgm_a is maternal genome for males in population a
    #pgm_a is paternal genome for males in population a
    mgf_a, pgf_a, mgm_a, pgm_a = start_geno(td1_a,td2_a,td3_a,td4_a,N,20,1)
    
    #Calculating male phenotypes from the genomes
    @views mphens_a=reduce(hcat,[sum(pgm_a[:,:,i],dims=2).+sum(mgm_a[:,:,i],dims=2) for i in 1:(size(pgm_a)[3])])
    #Making sure no value is below 1. This is important for mathematical errors. 
    mphens_a[mphens_a.<1] .= 1
     #Calculating female phenotypes from the genomes
    @views fphens_a=reduce(hcat,[sum(pgf_a[:,:,i],dims=2).+sum(mgf_a[:,:,i],dims=2) for i in 1:(size(pgm_a)[3])])
    #Making sure no value is below 1. This is important for mathematical errors. 
    fphens_a[fphens_a.<1] .= 1

    ###Same thing but for Population B
    #Sperm number average for Population B taken from the above key
    muS_b=bst[bst[:,1] .== a .&& bst[:,2] .==rsc .&& bst[:,3].==tradeoff  ,4]
    #Female trait distriubtion
    td1_b = Normal(80/40,(2.5^2/40)^0.5)
    #Male trait distribution
    td2_b = Normal(80/40,(2.5^2/40)^0.5)
    #Sperm count distribution
    td3_b = Normal(muS_b[1]/40,(2.5^2/40)^0.5)
    #Ecological trait distribution
    td4_b = Normal(40/40,(2.5^2/40)^0.5)
    #td4_b = Normal(80/40,(2.5^2/40)^0.5) #Use this instead of the line above for ecological divergence as well (Scenario 2, 3)
     ###actually generating the genomes
    #mgf_b is maternal genome for females in population b
    #pgf_b is paternal genome for females in population b
    #mgm_b is maternal genome for males in population b
    #pgm_b is paternal genome for males in population b
    mgf_b, pgf_b, mgm_b, pgm_b = start_geno(td1_b,td2_b,td3_b,td4_b,N,20,2)

    #Calculating male phenotypes
    @views mphens_b=reduce(hcat,[sum(pgm_b[:,:,i],dims=2).+sum(mgm_b[:,:,i],dims=2) for i in 1:(size(pgm_b)[3])])
    #Making sure minimum value is 1
    mphens_b[mphens_b.<1] .= 1

    #Calculating female phenotypes
    @views fphens_b=reduce(hcat,[sum(pgf_b[:,:,i],dims=2).+sum(mgf_b[:,:,i],dims=2) for i in 1:(size(pgm_b)[3])])
    #Making sure minimum value is 1
    fphens_b[fphens_b.<1] .= 1

    ####Preallocating some vectors
    #contains the results for the simulation
    dfall=zeros(generations,83)
    #precopulatory success vector for population a
    precop_a=zeros(K)
    #weighted precopulatory probability vector for population a
    preprob_a=zeros(K)
     #precopulatory success vector for population b
    precop_b=zeros(K)
    #weighted precopulatory probability vector for population b
    preprob_b=zeros(K)
    #vector for indices of migrants
    migrants=fill(1,m)
    #vector for indices of migrants from previous generation
    Pmigrants=fill(1,m)
    #matrix of phenotypes of male migrants that is used to store when swapping them between population a and b
    Mtemp=zeros(m,5)
    #matrix of phenotypes of female migrants that is used to store when swapping them between population a and b
    Ftemp=zeros(m,5)
    #Vector of indices of Male survivors in population A
    Msurv_a=fill(1,K)
    #Vector of indices of Female survivors in population A
    Fsurv_a=fill(1,K)
    #Vector of indices of Male survivors in population B
    Msurv_b=fill(1,K)
    #Vector of indices of Female survivors in population B
    Fsurv_b=fill(1,K)
    #Vector of indices of Male survivors in population A from the previous generation
    PMsurv_a=fill(1,K)
    #Vector of indices of Female survivors in population A from the previous generation
    PFsurv_a=fill(1,K)
    #Vector of indices of Male survivors in population B from the previous generation
    PMsurv_b=fill(1,K)
    #Vector of indices of Female survivors in population B from the previous generation
    PFsurv_b=fill(1,K)
    #Maternal genome
    egg=zeros(1,size(pgf_a)[2],5)
    #Paternal genome 
    sperm=zeros(1,size(pgf_a)[2],5)
    #vector of summary statistics for population 
    sumdf =zeros(31)
    #Randomize vector for mixing up offspring
    #prevents lineage from staying in same vector position.
    nfi=collect(1:N)
    #Keeps track of parents for female offspring in population A
    FOffA=fill(0,(N,2))
    #Keeps track of parents for female offspring in population B
    FOffB=fill(0,(N,2))
    #Keeps track of parents for male offspring in population A
    MOffA=fill(0,(N,2))
    #Keeps track of parents for male offspring in population B
    MOffB=fill(0,(N,2))
    #Keeps track of parents for surviving female offspring in population A
    SFOffA=fill(0,(K,2))
    #Keeps track of parents for surviving female offspring in population B
    SFOffB=fill(0,(K,2))
    #Keeps track of parents for surviving male offspring in population A
    SMOffA=fill(0,(K,2))
    #Keeps track of parents for surviving male offspring in population B
    SMOffB=fill(0,(K,2))
    #Vector of fitness of sons for females in population A
    SonFitA=fill(0,N)
    #Vector of fitness of sons for females in population B
    SonFitB=fill(0,N)
    #Vector of mothers of male offspring in previous generation for population A
    PMOffA =fill(0,N)
    #Vector of mothers of male offspring in previous generation for population B
    PMOffB =fill(0,N)
    #Number of initial offspring for females in population A
    FFertA =fill(0,N)
    #Number of intial offspring for males in population A
    MFertA =fill(0,N)
    #Number of initial offspring for females in population B
    FFertB =fill(0,N)
    #Number of initial offspring for males in population B
    MFertB =fill(0,N)
    #Number of surviving offspring for females in population A 
    SFFertA =fill(0,N)
    #Number of surviving offspring for males in population A 
    SMFertA =fill(0,N)
    #Number of surviving offspring for females in population B
    SFFertB =fill(0,N)
    #Number of surviving offspring for females in population B
    SMFertB =fill(0,N)
    #Standardized cryptic female trait for population A
    Femalestnd_a =zeros(K)
    #standardized female traits squared for gamma selection coeffients of population A
    Femalestnd2_a =zeros(K)
    #Standardized cryptic female trait for population B
    Femalestnd_b =zeros(K)
    #standardized female traits squared for gamma selection coeffients of population B
    Femalestnd2_b =zeros(K)

    #standardized male sperm trait for selection analysis of population A
    Malestnd_a =zeros(K)
    #standardized male sperm trait squared for gamma selection coeffients of population A
    Malestnd2_a =zeros(K)
    #standardized sperm number for selection analysis of population A
    SMalestnd_a =zeros(K)
    #standardized sperm number for gamma selection coeffients of population A
    SMalestnd2_a =zeros(K)
    #gamma coeffient for male x sperm number of population A
    gms_a =zeros(K)

    #standardized male sperm trait for selection analysis of population B
    Malestnd_b =zeros(K)
    #standardized male sperm trait squared for gamma selection coeffients of population B
    Malestnd2_b =zeros(K)
    #standardized sperm number for selection analysis of population B
    SMalestnd_b=zeros(K)
    #standardized sperm number for gamma selection coeffients of population B
    SMalestnd2_b =zeros(K)
    #gamma coeffient for male x sperm number of population B
    gms_b =zeros(K)

    #Relative surviving offspring for females population A
    SreloffA=zeros(K)
    #Relative surviving offspring for females population B
    SreloffB =zeros(K)
    #Relative surviving offspring for males population A
    SreloffMA=zeros(K)
    #Relative surviving offspring for males population B
    SreloffMB=zeros(K)
    #data frame for female selection analyses of population A
    dfOA=DataFrame(RelFit=SreloffA,FMale=Femalestnd_a,FMaleq= Femalestnd2_a)
    #data frame for female selection analyses of population b
    dfOB=DataFrame(RelFit=SreloffB,FMale=Femalestnd_b,FMaleq= Femalestnd2_b)
    #data frame for male sleection analyses of population A
    dfOAM =DataFrame(RelFit=SreloffMA,Male=Malestnd_a,Maleq=Malestnd2_a,SMale=SMalestnd_a,SMaleq=SMalestnd2_a,MSq=gms_a)
    #data frame for male selection analyses of population B
    dfOBM = DataFrame(RelFit=SreloffMB,Male=Malestnd_b,Maleq=Malestnd2_b,SMale=SMalestnd_b,SMaleq=SMalestnd2_b,MSq=gms_b)
    ####Mating
    ##Population A
    #maternal genome females next gen
    mgf2_a=zeros(N,20,5)
    #paternal genome females next gen
    pgf2_a=zeros(N,20,5)
    #maternal genome males next gen
    mgm2_a=zeros(N,20,5)
    #paternal genome males next gen
    pgm2_a=zeros(N,20,5)
    ##Population B
    #maternal genome females next gen
    mgf2_b=zeros(N,20,5)
    #paternal genome females next gen
    pgf2_b=zeros(N,20,5)
    #maternal genome males next gen
    mgm2_b=zeros(N,20,5)
    #paternal genome males next gen
    pgm2_b=zeros(N,20,5)

    #Now for loop to simulate until specified generation.
    @inbounds @views for gen in 1:generations
        #note on indexing for the genotypes
        #pgm[row,column,other]
        #pgm[individual,loci,trait]
        #note on indexing for phenotypes
        #first column is female trait
        #second column is male trait
        #third column is sperm number
        #fourth column is ecological value
        #fifth column is neutral loci
        #Don't need to redo phenotypes if it is the first generation
        if gen == 1

        #otherwise recalculate phenoypes and reset offsrping to zero
        #.= reasigns variable without allocating more memory
        else
            #Population a 
            ## Male
            @views mphens_a .= reduce(hcat,[sum(pgm_a[:,:,i],dims=2).+sum(mgm_a[:,:,i],dims=2) for i in 1:(size(pgm_a)[3])])
            mphens_a[mphens_a.<1] .= 1
            #now make female phenotype array
            @views fphens_a=reduce(hcat,[sum(pgf_a[:,:,i],dims=2).+sum(mgf_a[:,:,i],dims=2) for i in 1:(size(pgm_a)[3])])
            fphens_a[fphens_a.<1] .= 1
            @views mphens_b .= reduce(hcat,[sum(pgm_b[:,:,i],dims=2).+sum(mgm_b[:,:,i],dims=2) for i in 1:(size(pgm_b)[3])])
            mphens_b[mphens_b.<1] .= 1
            #now make female phenotype array
            @views fphens_b .= reduce(hcat,[sum(pgf_b[:,:,i],dims=2).+sum(mgf_b[:,:,i],dims=2) for i in 1:(size(pgm_b)[3])])
            fphens_b[fphens_b.<1] .= 1
        end
        #MeanFemale_a,SDFemale_a,MeanMale_a,SDMale_a,MeanSperm_a,SDSperm_a,MeanE_a,SDE_a,MeanB_a,SDB_a,cor_mf_a,cor_me_a,cor_fe_a
        #MeanFemale_b,SDFemale_b,MeanMale_b,SDMale_b,MeanSperm_b,SDSperm_b,MeanE_b,SDE_b,MeanB_b,SDB_b,cor_mf_b,cor_me_b,cor_fe_b,
        #a,rsc,gen
        @views sumdf .=[mean(fphens_a[:,1]),std(fphens_a[:,1]),mean(mphens_a[:,2]),std(mphens_a[:,2]),
        mean(mphens_a[:,3]),std(mphens_a[:,3]),mean(cat(mphens_a[:,4],fphens_a[:,4],dims=1)),std(cat(mphens_a[:,4],fphens_a[:,4],dims=1)),
        mean(cat(mphens_a[:,5],fphens_a[:,5],dims=1)),std(cat(mphens_a[:,5],fphens_a[:,5],dims=1)),
        cor(cat(mphens_a[:,2],fphens_a[:,2],dims=1),cat(mphens_a[:,1],fphens_a[:,1],dims=1)),
        cor(cat(mphens_a[:,2],fphens_a[:,2],dims=1),cat(mphens_a[:,4],fphens_a[:,4],dims=1)),
        cor(cat(mphens_a[:,1],fphens_a[:,1],dims=1),cat(mphens_a[:,4],fphens_a[:,4],dims=1)),
        mean(fphens_b[:,1]),std(fphens_b[:,1]),mean(mphens_b[:,2]),std(mphens_b[:,2]),
        mean(mphens_b[:,3]),std(mphens_b[:,3]),mean(cat(mphens_b[:,4],fphens_b[:,4],dims=1)),std(cat(mphens_b[:,4],fphens_b[:,4],dims=1)),
        mean(cat(mphens_b[:,5],fphens_b[:,5],dims=1)),std(cat(mphens_b[:,5],fphens_b[:,5],dims=1)),
        cor(cat(mphens_b[:,2],fphens_b[:,2],dims=1),cat(mphens_b[:,1],fphens_b[:,1],dims=1)),
        cor(cat(mphens_b[:,2],fphens_b[:,2],dims=1),cat(mphens_b[:,4],fphens_b[:,4],dims=1)),
        cor(cat(mphens_b[:,1],fphens_b[:,1],dims=1),cat(mphens_b[:,4],fphens_b[:,4],dims=1)),
        m,an,a,rsc,gen]
        #### Migration
        #randomly select indices that will be the migrants
        migrants.=sample(1:size(mphens_a)[1],m,replace=false)
        #sort migrants based on indecies for faster extraction later on
        sort!(migrants)
        #Temporary phenotypes for the swap
        #storing the migrants from population A
        Mtemp .= mphens_a[migrants,:]
        Ftemp .= fphens_a[migrants,:]
        #assign male pop a to male pop b
        mphens_a[migrants,:]=mphens_b[migrants,:]
        #assign male pop b to male pop a
        mphens_b[migrants,:]=Mtemp
        #assign female pop a to female pop b
        fphens_a[migrants,:]=fphens_b[migrants,:]
        #assign female pop b to female pop a
        fphens_b[migrants,:]=Ftemp
        #### Survival (change last parameter to specific ecological adaptive peak)
        #Population A
        prob_surv!(fphens_a,an,40)
        prob_surv!(mphens_a,an,40)
        #extracting indices of male survivors for population A
        Msurv_a .= findall(!isnan,mphens_a[:,1])
        #extracting indices of female survivors for population A
        Fsurv_a .= findall(!isnan,fphens_a[:,1])
        #Population B
        prob_surv!(fphens_b,an,40)
        prob_surv!(mphens_b,an,40)
        #extracting indices of male survivors for population B
        Msurv_b .= findall(!isnan,mphens_b[:,1])
        #extracting indices of female survivors for population B
        Fsurv_b .= findall(!isnan,fphens_b[:,1])
        ###Fitness of previous generation using surviving offspring as a proxy
        #because it relies on the previous generation can't do this for generaiton 1 and won't be avaliable for final generation
        if gen > 1
            ###excluding migranting offspring for the sake of caluclation as they are "lost" from the population
            #extracting Surviving offspring for females of population A
            SFOffA = FOffA[setdiff(findall(!isnan,fphens_a[:,1]),migrants),:]
            #extracting Surviving offspring for females of population B
            SFOffB = FOffB[setdiff(findall(!isnan,fphens_b[:,1]),migrants),:]
            #extrating Surviving offspring for males of population A
            SMOffA = MOffA[setdiff(findall(!isnan,mphens_a[:,1]),migrants),:]
            #extracting Surviving offspring for males of population B
            SMOffB = MOffB[setdiff(findall(!isnan,mphens_b[:,1]),migrants),:]
            #Fitness proxied by surviving individuals 
            #Female fitness A
            SFFertA .= countmemb(SFOffA[:,1],N) .+ countmemb(SMOffA[:,1],N)
            #Male fitness A
            SMFertA .= countmemb(SFOffA[:,2],N) .+ countmemb(SMOffA[:,2],N)
            #Female fitness B
            SFFertB .= countmemb(SFOffB[:,1],N) .+ countmemb(SMOffB[:,1],N)
            #Male fintess B
            SMFertB .= countmemb(SFOffB[:,2],N) .+ countmemb(SMOffB[:,2],N)

            #### Number of surviving offspring for past generation migrants
            #Female migrants of population A
            SFRawAM = sum(SFFertA[Pmigrants,:])
            #Male migrants of population A
            SMRawAM = sum(SMFertA[Pmigrants,:])
            #Female migrants of population B
            SFRawBM = sum(SFFertB[Pmigrants,:])
            #Male migrants of population B
            SMRawBM = sum(SMFertB[Pmigrants,:])
            #### Number of surviving offspring for past generation non-migrants
            #deleteat! is a function that takes the inverse indices, in this case inverse of past migrants
            #Female non-migrants population A
            SFRawAN = sum(SFFertA[deleteat!(collect(1:N),Pmigrants),:])
            #Male non-migrants population A
            SMRawAN = sum(SMFertA[deleteat!(collect(1:N),Pmigrants),:])
            #Female non-migrants population B
            SFRawBN = sum(SFFertB[deleteat!(collect(1:N),Pmigrants),:])
            #Male non-migrants population B
            SMRawBN = sum(SMFertB[deleteat!(collect(1:N),Pmigrants),:]) 
            #save these results into the data frame
            dfall[(gen-1),48:55]=[SFRawAM,SMRawAM,SFRawBM,SMRawBM,SFRawAN,SMRawAN,SFRawBN,SMRawBN]
            #####Selection analyses now using surviving offspring
            #Surviving offspring
            ###Female A
            #calculate relative number of surviving offspring
            #Only factoring in females that survived
            SreloffA .= SFFertA[PFsurv_a]./(sum(SFFertA)/K)
            #make data frame to calculate selection coeffients
            dfOA .= DataFrame(RelFit=SreloffA,FMale=Femalestnd_a,FMaleq= Femalestnd2_a)
            #calculate selection coeffients
            modelA=lm(@formula(RelFit ~ FMale+FMaleq),dfOA)
            #beta estimate for females population a
            SFCB_a=coef(modelA)[2]
            #gamma estimate for females population a
            SFCG_a=coef(modelA)[3]
            ###Female B
            #calculate relative number of surviving offspring
            #Only factoring in females that survived
            SreloffB .= SFFertB[PFsurv_b]./(sum(SFFertB)/K)
            #make data frame to calculate selection coeffients
            dfOB.=DataFrame(RelFit=SreloffB,FMale=Femalestnd_b,FMaleq= Femalestnd2_b)
            #calculate selection coeffients
            modelB=lm(@formula(RelFit ~ FMale+FMaleq),dfOB)
            #beta estimte for for females population B
            SFCB_b=coef(modelB)[2]
            #gamma estiamte for females population B
            SFCG_b=coef(modelB)[3]
            ###Male A
            SreloffMA .= SMFertA[PMsurv_a]./(sum(SMFertA)/K)
            #make data frame to calculate selection coeffients
            dfOAM .=DataFrame(RelFit=SreloffMA,Male=Malestnd_a,Maleq=Malestnd2_a,SMale=SMalestnd_a,SMaleq=SMalestnd2_a,MSq=gms_a)
            #calculate selection coeffients
            modelMA=lm(@formula(RelFit ~ Male+ Maleq+SMale+SMaleq+MSq),dfOAM)
            #Beta for male sperm trait
            SMCB_a=coef(modelMA)[2]
            #Gamma for male sperm trait
            SMCG_a=coef(modelMA)[3]
            #Beta for sperm number
            SSCB_a=coef(modelMA)[4]
            #Gamma for sperm number
            SSCG_a=coef(modelMA)[5]
            #Gamma for sperm number x sperm trait
            SMSCG_a=coef(modelMA)[6]
            ###Male B
            SreloffMB .= SMFertB[PMsurv_b]./(sum(SMFertB)/K)
            #make data frame to calculate selection coeffients
            dfOBM .=DataFrame(RelFit=SreloffMB,Male=Malestnd_b,Maleq=Malestnd2_b,SMale=SMalestnd_b,SMaleq=SMalestnd2_b,MSq=gms_b)
            #calculate selection coeffients
            modelMB=lm(@formula(RelFit ~ Male+ Maleq+SMale+SMaleq+MSq),dfOBM)
            #Beta for male sperm trait
            SMCB_b=coef(modelMB)[2]
            #Gamma for male sperm trait
            SMCG_b=coef(modelMB)[3]
            #Beta for sperm number
            SSCB_b=coef(modelMB)[4]
            #Gamma for sperm number
            SSCG_b=coef(modelMB)[5]
            #Gamma for sperm number x sperm trait
            SMSCG_b=coef(modelMB)[6]
            ######Sexual Selection
            #repeat analysis for males but only use the fertilization sucess not factoring in survival of offspring
            ###Male A
            #relative fitness of offspring produced not just surviving offspring
            SreloffMA .=  MFertA[PMsurv_a]./(sum(MFertA)/K)
            #make data frame to calculate selection coeffients
            #overwriting the previous data frame
            dfOAM .=DataFrame(RelFit=SreloffMA,Male=Malestnd_a,Maleq=Malestnd2_a,SMale=SMalestnd_a,SMaleq=SMalestnd2_a,MSq=gms_a)
            #calculate selection coeffients
            modelMAS=lm(@formula(RelFit ~ Male+ Maleq+SMale+SMaleq+MSq),dfOAM)
            #Beta for male sperm trait
            SMCB_aS=coef(modelMAS)[2]
            #gamma fro male sperm trait
            SMCG_aS=coef(modelMAS)[3]
            #beta for sperm number
            SSCB_aS=coef(modelMAS)[4]
            #gamma for sperm number
            SSCG_aS=coef(modelMAS)[5]
            #Gamma for sperm number x sperm trait
            SMSCG_aS=coef(modelMAS)[6]
            ###Male B
            SreloffMB .= MFertB[PMsurv_b]./(sum(MFertB)/K)
            #make data frame to calculate selection coeffients
            dfOBM .= DataFrame(RelFit=SreloffMB,Male=Malestnd_b,Maleq=Malestnd2_b,SMale=SMalestnd_b,SMaleq=SMalestnd2_b,MSq=gms_b)
            #calculate selection coeffients
            modelMBS=lm(@formula(RelFit ~ Male+ Maleq+SMale+SMaleq+MSq),dfOBM)
            #Beta for male sperm trait
            SMCB_bS=coef(modelMBS)[2]
            #Gamma for male sperm trait
            SMCG_bS=coef(modelMBS)[3]
            #Beta for sperm number
            SSCB_bS=coef(modelMBS)[4]
            #Gamma for sperm number
            SSCG_bS=coef(modelMBS)[5]
            #Gamma for sperm number x sperm trait
            SMSCG_bS=coef(modelMBS)[6]
        end
        ####standardizing traits for selection analyses later on.
        #standardized based on surviving individuals
        #doing this before ejaculate depletion in males.
        #Female A
        Femalestnd_a .= (fphens_a[Fsurv_a,1] .- mean(fphens_a[Fsurv_a,1]))/std(fphens_a[Fsurv_a,1])
        #standardized female traits squared for gamma selection coeffients
        Femalestnd2_a .=0.5 .* Femalestnd_a .^ 2

        #Female B
        Femalestnd_b .=(fphens_b[Fsurv_b,1] .- mean(fphens_b[Fsurv_b,1]))/std(fphens_b[Fsurv_b,1])
        #standardized female traits squared for gamma selection coeffients
        Femalestnd2_b .=0.5 .* Femalestnd_b .^ 2
        
        #Male A
        #standardized male traits for selection analysis
        Malestnd_a .=(mphens_a[Msurv_a,2] .- mean(mphens_a[Msurv_a,2]))/std(mphens_a[Msurv_a,2])
        #standardized male traits squared for gamma selection coeffients
        Malestnd2_a .=0.5 .* Malestnd_a .^ 2
        #standardized sperm number for selection analysis
        SMalestnd_a .=(mphens_a[Msurv_a,3] .- mean(mphens_a[Msurv_a,3]))/std(mphens_a[Msurv_a,3])

        #standardized sperm number for gamma selection coeffients
        SMalestnd2_a .=0.5 .* SMalestnd_a .^ 2
    
        #gamma coeffient for male x sperm number
        gms_a .=Malestnd_a.*SMalestnd_a

        #Male B
        #standardized male traits for selection analysis
        Malestnd_b .=(mphens_b[Msurv_b,2] .- mean(mphens_b[Msurv_b,2]))/std(mphens_b[Msurv_b,2])
    
        #standardized male traits squared for gamma selection coeffients
        Malestnd2_b .=0.5 .* Malestnd_b .^ 2
    
        #standardized sperm number for selection analysis
        SMalestnd_b .=(mphens_b[Msurv_b,3] .- mean(mphens_b[Msurv_b,3]))/std(mphens_b[Msurv_b,3])
    
        #standardized sperm number for gamma selection coeffients
        SMalestnd2_b .=0.5 .* SMalestnd_b .^ 2
        #gamma coeffient for male x sperm number
        gms_b .= Malestnd_b.*SMalestnd_b
        
        #### Mating
        #if tradeoff weight probability of precop sucess by both male phenotype and sperm number (eq.1 in text)
        if tradeoff
            #population a
            precop_a .= mate.((mphens_a[Msurv_a,3].*mphens_a[Msurv_a,2]),1/1000,2500)
            preprob_a .=precop_a./sum(precop_a)
            #Population b
            precop_b .= mate.((mphens_b[Msurv_b,3].*mphens_b[Msurv_b,2]),1/1000,2500)
            preprob_b .=precop_b./sum(precop_b)
        #if not a tradeoff weight probability of precop success by sperm number (eq. 2 in text)
        else
            #precop=fill(1,length(mphens[:,3]))
            #population a
            precop_a .= mate.(mphens_a[Msurv_a,3],1/20,50)
            preprob_a .=precop_a./sum(precop_a)
            #population b
            precop_b .= mate.(mphens_b[Msurv_b,3],1/20,50)
            preprob_b .= precop_b./sum(precop_b)
        end
        #randomizing offspring indices so maternal lineage isn't always in the same index.
        shuffle!(nfi)
        #Population A
        #keep count of female offspring indexing
        fcount =1

        #keep count of male offspring indexing
        mcount =1
        #Make everything zero again for fitness
        #Just offspring
        FFertA .=fill(0,N)
        MFertA .=fill(0,N)
        FFertB .=fill(0,N)
        MFertB .=fill(0,N)
        #Surviving offspring
        SFFertA .=fill(0,N)
        SMFertA .=fill(0,N)
        SFFertB .=fill(0,N)
        SMFertB .=fill(0,N)
        #loop through all females to mate and reproduce for Population A
        for i in Fsurv_a
            #mates = number of males a female mates with
            #if/else statments determine that based on rsc (risk of sperm competiton)
            if rsc<=1
                mates =wsample([1,2],[(1-rsc),rsc],1)[1]
            elseif rsc<=2
                mates =wsample([2,3],[(2-rsc),rsc-1],1)[1]
            else
                mates =Int(rsc)
            end
            #sample from male population to get males female mates with
            #weighted by precopulatory sucess calculated above
            matesM=wsample(Msurv_a,preprob_a,mates,replace=false)
            #if there is only one male no need to model risk of sperm competition
            if mates==1
                #add 2 offspring to index of male
                #offspring_A[matesM[1]]=offspring[matesM[1]]+2
                #model sperm depletion of male after mating
                mphens_a[matesM,3]=mphens_a[matesM,3].*exp.(-0.2)
                #loop through number of offspring
                #could be extended beyond 2 which is why it is
                #in a for loop format
                for k in 1:4
                    #get dad's index for make_gamete function
                    dad=matesM[1]
                    if k >2 #make females
                        #make egg
                        #check if migrant
                        if i in migrants
                            egg.=make_gamete(pgf_b,mgf_b,i)
                        else
                            egg.=make_gamete(pgf_a,mgf_a,i)
                        end
                        #make sperm
                        if dad in migrants
                            sperm.=make_gamete(pgm_b,mgm_b,dad)
                        else
                            sperm.=make_gamete(pgm_a,mgm_a,dad)
                        end
                        #save paternal genome for female in index based on NFI
                        pgf2_a[nfi[fcount],:,:]=sperm
                        #save maternal genome for female
                        mgf2_a[nfi[fcount],:,:]=egg
                        #save mom and father of offspring in female offspring matrix
                        FOffA[nfi[fcount],:] =[i,dad]
                        #updated female counter
                        fcount+=1
                    else #make males
                        #make egg
                        #check if migrant
                        if i in migrants
                            egg.=make_gamete(pgf_b,mgf_b,i)
                        else
                            egg.=make_gamete(pgf_a,mgf_a,i)
                        end
                        #make sperm
                        if dad in migrants
                            sperm.=make_gamete(pgm_b,mgm_b,dad)
                        else
                            sperm.=make_gamete(pgm_a,mgm_a,dad)
                        end
                        #save paternal genome for male
                        pgm2_a[nfi[mcount],:,:]=sperm
                        #save maternal genome for male
                        mgm2_a[nfi[mcount],:,:]=egg
                        #save mom and father of offspring in male offspring matrix
                        MOffA[nfi[mcount],:] =[i,dad]
                        #increase male index by 1
                        mcount+=1
                    end
                end
            else
                #check if running selection analysis not cfc
                if d<0
                    #calcualte probs of fertilization sucess for males
                    probm=prob_success(mphens_a,a,fphens_a[i,1],matesM)
                    #fair raffle uncomment line below 
                    #probm=prob_successFR(mphens[matesM,3])
                    #deplete ejaculation for males
                    mphens_a[matesM,3]=mphens_a[matesM,3].*exp.(-0.2)
                else
                    #calculate probs of fertilization sucess for males for cryptic female choice
                    probm=prob_success(mphens_a,a,d,matesM)
                    #probm=prob_successFR(mphens[matesM,3])
                    #deplete ejaculation for males
                    mphens_a[matesM,3]=mphens[matesM,3].*exp.(-0.2)
                end
                #calculate who got fertilization sucess based on probs
                ferts=wsample(matesM,probm,4)
                #specific female is i
                #specific male is in the vector ferts
                for k in 1:4
                    dad=ferts[k]
                    if k >2 #make females
                        #make egg
                        #check if migrant
                        if i in migrants
                            egg.=make_gamete(pgf_b,mgf_b,i)
                        else
                            egg.=make_gamete(pgf_a,mgf_a,i)
                        end
                        #make sperm
                        if dad in migrants
                            sperm.=make_gamete(pgm_b,mgm_b,dad)
                        else
                            sperm.=make_gamete(pgm_a,mgm_a,dad)
                        end
                        #add paternal genome  for female
                        pgf2_a[nfi[fcount],:,:]=sperm
                        #add maternal genome for female
                        mgf2_a[nfi[fcount],:,:]=egg
                        #save mom and father of offspring in female offspring matrix
                        FOffA[nfi[fcount],:] =[i,dad]
                        #increase index counter for female offspring
                        fcount+=1
                    else #make males
                        #make egg
                        if i in migrants
                            egg.=make_gamete(pgf_b,mgf_b,i)
                        else
                            egg.=make_gamete(pgf_a,mgf_a,i)
                        end
                        #make sperm
                        if dad in migrants
                            sperm.=make_gamete(pgm_b,mgm_b,dad)
                        else
                            sperm.=make_gamete(pgm_a,mgm_a,dad)
                        end
                        #make paternal genome for male
                        pgm2_a[nfi[mcount],:,:]=sperm
                        #make maternal genome for male
                        mgm2_a[nfi[mcount],:,:]=egg
                        #save mom and father of offspring in male offspring matrix
                        MOffA[nfi[mcount],:] =[i,dad]
                        #increase index counter for male offspring
                        mcount+=1
                    end
                end
            end
        end
        #Population B
        #keep count of female offspring indexing
        fcount =1

        #keep count of male offspring indexing
        mcount =1
        for i in Fsurv_b
            #mates = number of males a female mates with
            #if/else statments determine that based on rsc (risk of sperm competiton)
            if rsc<=1
                mates =wsample([1,2],[(1-rsc),rsc],1)[1]
            elseif rsc<=2
                mates =wsample([2,3],[(2-rsc),rsc-1],1)[1]
            else
                mates =Int(rsc)
            end
            #sample from male population to get males female mates with
            #weighted by precopulatory sucess calculated above
            matesM=wsample(Msurv_b,preprob_b,mates,replace=false)
            #if there is only one male no need to model risk of sperm competition
            if mates==1
                #add 2 offspring to index of male
                #offspring_A[matesM[1]]=offspring[matesM[1]]+2
                #model sperm depletion of male after mating
                mphens_b[matesM,3]=mphens_b[matesM,3].*exp.(-0.2)
                #loop through number of offspring
                #could be extended beyond 2 which is why it is
                #in a for loop format
                for k in 1:4
                    #get dad's index for make_gamete function
                    dad=matesM[1]
                    if k >2 #make females
                        #make egg
                        #check if migrant
                        if i in migrants
                            egg.=make_gamete(pgf_a,mgf_a,i)
                        else
                            egg.=make_gamete(pgf_b,mgf_b,i)
                        end
                        #make sperm
                        if dad in migrants
                            sperm.=make_gamete(pgm_a,mgm_a,dad)
                        else
                            sperm.=make_gamete(pgm_b,mgm_b,dad)
                        end
                        #save paternal genome for female
                        pgf2_b[nfi[fcount],:,:]=sperm
                        #save maternal genome for female
                        mgf2_b[nfi[fcount],:,:]=egg
                        #save mom and father of offspring in female offspring matrix
                        FOffB[nfi[fcount],:] =[i,dad]
                        #updated female counter
                        fcount+=1
                    else #make males
                        #make egg
                        #check if migrant
                        if i in migrants
                            egg.=make_gamete(pgf_a,mgf_a,i)
                        else
                            egg.=make_gamete(pgf_b,mgf_b,i)
                        end
                        #make sperm
                        if dad in migrants
                            sperm.=make_gamete(pgm_a,mgm_a,dad)
                        else
                            sperm.=make_gamete(pgm_b,mgm_b,dad)
                        end
                        #save paternal genome for male
                        pgm2_b[nfi[mcount],:,:]=sperm
                        #save maternal genome for male
                        mgm2_b[nfi[mcount],:,:]=egg
                        #save mom and father of offspring in male offspring matrix
                        MOffB[nfi[mcount],:] =[i,dad]
                        #increase male index by 1
                        mcount+=1
                    end
                end
            else
                #check if running selection analysis not cfc
                if d<0
                    #calcualte probs of fertilization sucess for males
                    probm=prob_success(mphens_b,a,fphens_b[i,1],matesM)
                    #fair raffle uncomment line below 
                    #probm=prob_successFR(mphens[matesM,3])
                    #deplete ejaculation for males
                    mphens_b[matesM,3]=mphens_b[matesM,3].*exp.(-0.2)
                else
                    #calculate probs of fertilization sucess for males for cryptic female choice
                    probm=prob_success(mphens_b,a,d,matesM)
                    #probm=prob_successFR(mphens[matesM,3])
                    #deplete ejaculation for males
                    mphens_b[matesM,3]=mphens[matesM,3].*exp.(-0.2)
                end
                #calculate who got fertilization sucess based on probs
                ferts=wsample(matesM,probm,4)
                #specific female is i
                #specific male is in the vector ferts
                for k in 1:4
                    dad=ferts[k]
                    if k >2 #make females
                        #make egg
                        #check if migrant
                        if i in migrants
                            egg.=make_gamete(pgf_a,mgf_a,i)
                        else
                            egg.=make_gamete(pgf_b,mgf_b,i)
                        end
                        #make sperm
                        if dad in migrants
                            sperm.=make_gamete(pgm_a,mgm_a,dad)
                        else
                            sperm.=make_gamete(pgm_b,mgm_b,dad)
                        end
                        #add paternal genome  for female
                        pgf2_b[nfi[fcount],:,:]=sperm
                        #add maternal genome for female
                        mgf2_b[nfi[fcount],:,:]=egg
                        #save mom and father of offspring in female offspring matrix
                        FOffB[nfi[fcount],:] =[i,dad]
                        #increase index counter for female offspring
                        fcount+=1
                    else #make males
                        #make egg
                        if i in migrants
                            egg.=make_gamete(pgf_a,mgf_a,i)
                        else
                            egg.=make_gamete(pgf_b,mgf_b,i)
                        end
                        #make sperm
                        if dad in migrants
                            sperm.=make_gamete(pgm_a,mgm_a,dad)
                        else
                            sperm.=make_gamete(pgm_b,mgm_b,dad)
                        end
                        #make paternal genome for male
                        pgm2_b[nfi[mcount],:,:]=sperm
                        #make maternal genome for male
                        mgm2_b[nfi[mcount],:,:]=egg
                        #save mom and father of offspring in male offspring matrix
                        MOffB[nfi[mcount],:] =[i,dad]
                        #increase index counter for male offspring
                        mcount+=1
                    end
                end
            end
        end
        #put all model results together
        #mean male,mean female, std male, std female,cor,sperm count, sperm count std,is,int,beta,gamma,A,RSC,generation
        ## next generation
        #Population a
        pgm_a .= pgm2_a
        mgm_a .= mgm2_a
        pgf_a .= pgf2_a
        mgf_a .= mgf2_a
        #Population b
        pgm_b .= pgm2_b
        mgm_b .= mgm2_b
        pgf_b .= pgf2_b
        mgf_b .= mgf2_b
        #Female fitness A
        FFertA .= countmemb(FOffA[:,1],N) .+ countmemb(MOffA[:,1],N)
        #Male fitness A
        MFertA .= countmemb(FOffA[:,2],N) .+ countmemb(MOffA[:,2],N)
        #Female fitness B
        FFertB .= countmemb(FOffB[:,1],N) .+ countmemb(MOffB[:,1],N)
        #Male fintess B
        MFertB .= countmemb(FOffB[:,2],N) .+ countmemb(MOffB[:,2],N)
        ###Fitness of sons for sexy sons
        if gen>1
            ##Pop A
            #Counting fitness of sons for each female in the previous generation for population A 
            SonFitA .=countmemb2(PMOffA,MFertA,N,migrants)./countmemb3(PMOffA,MFertA,N,Msurv_a,migrants)
            #Calculating relative fitness
            #need to account for only surviving females (mothers)
            SreloffA .= SonFitA[PFsurv_a]./mean(SonFitA[PFsurv_a][isfinite.(SonFitA[PFsurv_a])])
            #make data frame to calculate selection coeffients
            #Just reusing the old date frame with the right phenotypes but replacing the fitness measurement
            #need to do this because I overwrite the phenotypes elsewhere with current generation.
            dfOA[:,1] = SreloffA
            #calculate selection coeffients
            modelSonA=lm(@formula(RelFit ~ FMale+FMaleq),dfOA[isfinite.( dfOA[:,1]),:])
            #save B estimate for linear selection
            SFCB_aS=coef(modelSonA)[2]
            #save Gamma for quadratic selection
            SFCG_aS=coef(modelSonA)[3]

            ###Female B
            #Counting fitness of sons for each female in the previous generation for population B
            SonFitB .= countmemb2(PMOffB,MFertB,N,migrants)./countmemb3(PMOffB,MFertB,N,Msurv_b,migrants)
            #need to account for only surviving females (mothers)
            SreloffB .= SonFitB[PFsurv_b]./mean(SonFitB[PFsurv_b][isfinite.(SonFitB[PFsurv_b])])
            #make data frame to calculate selection coeffients
            #Just reusing the old date frame with the right phenotypes but replacing the fitness measurement
            #need to do this because I overwrite the phenotypes elsewhere with current generation.
            dfOB[:,1] = SreloffB
            #calculate selection coeffients
            modelSonB=lm(@formula(RelFit ~ FMale+FMaleq),dfOB[isfinite.( dfOB[:,1]),:])
            #save B estimate for linear selection
            SFCB_bS=coef(modelSonB)[2]
            #save Gamma for quadratic selection
            SFCG_bS=coef(modelSonB)[3]

            #Adding all selection estimates
            dfall[(gen-1),56:83]= [SFCB_a,SFCG_a,SFCB_b,SFCG_b,
            SFCB_aS,SFCG_aS,SFCB_bS,SFCG_bS,
            SMCB_a,SMCG_a,SSCB_a,SSCG_a,SMSCG_a,SMCB_b,SMCG_b,SSCB_b,SSCG_b,SMSCG_b,
            SMCB_aS,SMCG_aS,SSCB_aS,SSCG_aS,SMSCG_aS,SMCB_bS,SMCG_bS,SSCB_bS,SSCG_bS,SMSCG_bS]
        end
        #### Survial migrants
        #Number of male migrants that survived in population A
        MSurvAM=length(intersect(migrants,Msurv_a))
        #Number of female migrants that survived in population A
        FSurvAM=length(intersect(migrants,Fsurv_a))
        #Number of male migrants that survived in population B
        MSurvBM=length(intersect(migrants,Msurv_b))
        #Number of female migrants that survived in population B
        FSurvBM=length(intersect(migrants,Fsurv_b))
        #### Survival non-migrants
        ##deleteat! function is a fast way to get all non migrant indices
        #Number of male non-migrants that survived in population A
        MSurvAN=length(intersect(deleteat!(collect(1:N),sort(migrants)),Msurv_a))
        #Number of female non-migrants that survived in population A
        FSurvAN=length(intersect(deleteat!(collect(1:N),sort(migrants)),Fsurv_a))
        #Number of male non-migrants that survived in population B
        MSurvBN=length(intersect(deleteat!(collect(1:N),sort(migrants)),Msurv_b))
        #Number of female non-migrants that survived in population B
        FSurvBN=length(intersect(deleteat!(collect(1:N),sort(migrants)),Fsurv_b))
        #### Raw Fitness migrants
        FRawAM = sum(FFertA[migrants,:])
        MRawAM = sum(MFertA[migrants,:])
        FRawBM = sum(FFertB[migrants,:])
        MRawBM = sum(MFertB[migrants,:])
        #### Raw Fitness non-migrants
        FRawAN = sum(FFertA[deleteat!(collect(1:N),migrants),:])
        MRawAN = sum(MFertA[deleteat!(collect(1:N),migrants),:])
        FRawBN = sum(FFertB[deleteat!(collect(1:N),migrants),:])
        MRawBN = sum(MFertB[deleteat!(collect(1:N),migrants),:])
        #Storing summary values of generation  
        dfall[gen,1:31]=sumdf
        #storing information about survival and reproductive success of migrants and non migrants
        dfall[gen,32:47]=[MSurvAM,FSurvAM,MSurvBM,FSurvBM,MSurvAN,FSurvAN,MSurvBN,FSurvBN,
        FRawAM,MRawAM ,FRawBM ,MRawBM ,FRawAN ,MRawAN ,FRawBN ,MRawBN]
        ##Saving previous values for calculations of surviving offspring in the next generation.
        #migrants in soon to be previous generation
        Pmigrants .= migrants
        #Male survivors population A in soon to be previous generation
        PMsurv_a .= Msurv_a
        #Female survivors population A in soon to be previous generation
        PFsurv_a .= Fsurv_a
        #Male survivors population B in soon to be previous generation
        PMsurv_b .=Msurv_b 
        #Female survivors population B in soon to be previous generation
        PFsurv_b .=Fsurv_b
        #Getting mothers of previous generation
        #Mothers of population A
        PMOffA .=MOffA[:,1]
        #Mothers of Population B
        PMOffB .=MOffB[:,1]
    end
    return(dfall)
end


#N,m,an,a,rsc,tradeoff,generations,d=-1
#function or run simulation so I can put it in a for loop below


@everywhere function runsim(reps,N,m,an,a,rsc,tradeoff,gens,d=-1)
  resultsP=SharedArray{Float64}(reps*gens,84)
  @sync @distributed for i in 1:reps
    @async resultsP[(1+(i-1)*gens):(gens*i),1:83]=sim(N,m,an,a,rsc,tradeoff,gens,d)
    @async resultsP[(1+(i-1)*gens):(gens*i),84]=fill(i,gens)
  end
  return(resultsP)
end


@everywhere cd("YourResultsDirectoryPath")

#Change size of results allocation to number of reps * generation
results = zeros(150000,84)
data = DataFrame(results,["Mean.Female_a","SD.Female_a","Mean.Male_a","SD.Male_a","Mean.Sperm_a","SD.Sperm_a","Mean.E_a","SD.E_a","Mean.B_a","SD.B_a",
"cor.mf_a","cor.me_a","cor.fe_a","Mean.Female_b","SD.Female_b","Mean.Male_b","SD.Male_b","Mean.Sperm_b","SD.Sperm_b","Mean.E_b","SD.E_b","Mean.B_b","SD.B_b",
"cor.mf_b","cor.me_b","cor.fe_b","m","an","a","rsc","Generation",
"Surv_Male_Mig_a","Surv_Female_Mig_a","Surv_Male_Mig_b","Surv_Female_Mig_b","Surv_Male_NMig_a","Surv_Female_NMig_a","Surv_Male_NMig_b","Surv_Female_NMig_b",
"F_Off_Mig_a","M_Off_Mig_a","F_Off_Mig_b","M_Off_Mig_b","F_Off_NMig_a","M_Off_NMig_a","F_Off_NMig_b","M_Off_NMig_b",
"F_SurvOff_Mig_a","M_SurvOff_Mig_a","F_SurvOff_Mig_b","M_SurvOff_Mig_b","F_SurvOff_NMig_a","M_SurvOff_NMig_a","F_SurvOff_NMig_b","M_SurvOff_NMig_b",
"F_SurvOff_Beta_a","F_SurvOff_Gamma_a","F_SurvOff_Beta_b","F_SurvOff_Gamma_b",
"F_SonOff_Beta_a","F_SonOff_Gamma_a","F_SonOff_Beta_b","F_SonOff_Gamma_b",
"M_SurvOff_Beta_a","M_SurvOff_Gamma_a","S_SurvOff_Beta_a","S_SurvOff_Gamma_a","SM_SurvOff_Gamma_a",
"M_SurvOff_Beta_b","M_SurvOff_Gamma_b","S_SurvOff_Beta_b","S_SurvOff_Gamma_b","SM_SurvOff_Gamma_b",
"M_Off_Beta_a","M_Off_Gamma_a","S_Off_Beta_a","S_Off_Gamma_a","SM_Off_Gamma_a",
"M_Off_Beta_b","M_Off_Gamma_b","S_Off_Beta_b","S_Off_Gamma_b","SM_Off_Gamma_b","Rep"])

#run various simulations changing
#m: migration number
#an: strength of natural selection
#j: prescence/absence of a tradeoff between sperm number and sperm trait
#k: stenght of cryptic preference 
#l: risk or intesnity of sperm competition
#in runsim(50,1000,m,an,k,l,j,30000)
##50 is number of replicates
#1000 is 1/2 popualtion size or population size of males, and females (so this makes 1000 males and 1000 females)
#30000 is how many generations.
for m in [50]
    for an in [1]
        for j in [false]
          for k in [1,12.5,50]
            # 3,4,5,6,7,8,9,10
            for l in [3,4,5,6,7,8,9,10]
            results .= runsim(50,5000,m,an,k,l,j,3000)
            data .= DataFrame(results,["Mean.Female_a","SD.Female_a","Mean.Male_a","SD.Male_a","Mean.Sperm_a","SD.Sperm_a","Mean.E_a","SD.E_a","Mean.B_a","SD.B_a",
            "cor.mf_a","cor.me_a","cor.fe_a","Mean.Female_b","SD.Female_b","Mean.Male_b","SD.Male_b","Mean.Sperm_b","SD.Sperm_b","Mean.E_b","SD.E_b","Mean.B_b","SD.B_b",
            "cor.mf_b","cor.me_b","cor.fe_b","m","an","a","rsc","Generation",
            "Surv_Male_Mig_a","Surv_Female_Mig_a","Surv_Male_Mig_b","Surv_Female_Mig_b","Surv_Male_NMig_a","Surv_Female_NMig_a","Surv_Male_NMig_b","Surv_Female_NMig_b",
            "F_Off_Mig_a","M_Off_Mig_a","F_Off_Mig_b","M_Off_Mig_b","F_Off_NMig_a","M_Off_NMig_a","F_Off_NMig_b","M_Off_NMig_b",
            "F_SurvOff_Mig_a","M_SurvOff_Mig_a","F_SurvOff_Mig_b","M_SurvOff_Mig_b","F_SurvOff_NMig_a","M_SurvOff_NMig_a","F_SurvOff_NMig_b","M_SurvOff_NMig_b",
            "F_SurvOff_Beta_a","F_SurvOff_Gamma_a","F_SurvOff_Beta_b","F_SurvOff_Gamma_b",
            "F_SonOff_Beta_a","F_SonOff_Gamma_a","F_SonOff_Beta_b","F_SonOff_Gamma_b",
            "M_SurvOff_Beta_a","M_SurvOff_Gamma_a","S_SurvOff_Beta_a","S_SurvOff_Gamma_a","SM_SurvOff_Gamma_a",
            "M_SurvOff_Beta_b","M_SurvOff_Gamma_b","S_SurvOff_Beta_b","S_SurvOff_Gamma_b","SM_SurvOff_Gamma_b",
            "M_Off_Beta_a","M_Off_Gamma_a","S_Off_Beta_a","S_Off_Gamma_a","SM_Off_Gamma_a",
            "M_Off_Beta_b","M_Off_Gamma_b","S_Off_Beta_b","S_Off_Gamma_b","SM_Off_Gamma_b","Rep"])
            CSV.write(string("NoE_",j,"_",k,"_",l,"_",m,"_",an,".csv"),data)
            end
          end
        end
    end
end