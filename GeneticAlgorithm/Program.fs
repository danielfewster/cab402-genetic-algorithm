module CAB402.FSharp.GeneticAlgorithm

open RandomMonad

// The genes of one individual within the population. Each gene is an integer.
type Individual = int array

// The genes of an individual together with an assessment of the fitness of that individual
type ScoredIndividual = Individual * float 

// a collection of scored individuals that make up a generation
type Population = ScoredIndividual array

// Find an individual within the population that has the highest fitness
let fitest (population: Population) : ScoredIndividual = Array.maxBy (fun i -> snd i) population

// Given a set of competeting individuals, return the winning individual (i.e. one with best fitness)
let tournamentWinner (competitors: Population): Individual = fst (fitest competitors)

// Randomly select an individual from a population by conducting a tournament.
// A field of n = 2 competitors is first randomly generated and then the best individual within tht field is selected.
// Each of individuals selected for the competition is selected independently, 
// so it is possible that the same individual may be selected more than once.
let tournamentSelect (population: Population) : Rand<Individual> =
    rand {
        let! n1 = chooseRandom population
        let! n2 = chooseRandom population
        return tournamentWinner [| n1; n2 |]
    }

// Combine the genes of parent1 and parent2 based on the given splitPoint 
// The splitpoint will always be between 1 and length-1, where length is the length of both parent genes)
// The genes in position 0..splitPoint will come directly from the corresponding genes of parent1
// It is important the genes of the generated child is a legal permutation
// i.e. it should include each of the integers between 0 and length-1 precisely once.
// The order of the remaining genes of the child (those that were not inherited from parent1) are 
// determined by the order that they occurred in parent2.
// For example if parent1 = [0,3,5,4,2,1,6] and parent2 = [6,4,2,1,0,3,5] and the splitpoint is 4
// then the first 4 genes come from parent1 [0,3,5,4] and the remaining genes [2,1,6] are ordered 
// according to parent2 i.e. [6,2,1] because 6 comes before 2 and 2 comes before 1 in parent2.
// So the child in this example will be [0,3,5,4,6,2,1]
let crossAt (parent1: Individual) (parent2: Individual) (splitPoint: int): Individual =
    let left, right = parent1 |> Array.splitAt splitPoint
    let crossover = parent2 |> Array.filter (fun gene -> right |> Array.contains gene)
    Array.append left crossover

// Combine the genes of parent1 and parent2 at a randonly choosen splitpoint as per the above crossAt algorithm
// The splitpoint is chosen so that both parents provide at least one gene to the child
let cross (parent1: Individual) (parent2: Individual) : Rand<Individual> =
    rand {
        let! randomSplit = intRange 1 (parent1.Length-1)
        return crossAt parent1 parent2 randomSplit
    }

// Return a mutated version of the original genes
// the sequence of genes is split into 3 sections, a start, middle and end, based on the 2 provided indexes
// (where 0 <= firstIndex < secondIndex < genes.length)
// The start and end sections of the genes are left intact, while the genes in the middle section are reversed in order.
// For example reverseMutateAt [0,3,5,4,2,1,6] 2 4 = [0,3,2,4,5,1,6]
let reverseMutateAt (genes: Individual) (firstIndex: int) (secondIndex: int): Individual =
    let startSection = genes.[..(firstIndex-1)]
    let middleSection = genes.[firstIndex..secondIndex]
    let endSection = genes.[(secondIndex+1)..]
    Array.concat [| startSection; Array.rev middleSection; endSection |]

// Perform a reverse mutation based on two randomly chosen split points
// (such that 0 <= firstIndex < secondIndex < genes.length)
let reverseMutate (chromosome: Individual): Rand<Individual> =
    rand {
        let! first = intRange 0 (chromosome.Length-1)
        let! second = intRange (first+1) chromosome.Length
        return reverseMutateAt chromosome first second
    }

let MutateProbability = 0.15

// Perform a reverse mutation of the given genes with probability 0.15,
// otherwise leave the sequence unaltered.
let possiblyMutate (genes: Individual) : Rand<Individual> =
    rand {
        let! prob = withProbability MutateProbability
        if prob then return! reverseMutate genes 
        else return genes
    }

// Create a new population that consists of all of the children, plus the 10 best individuals from the previous generation.
let elitismSelection (parents: Population) (children: Population) : Population =    
    let bestParents = 
        parents
        |> Array.sortByDescending (fun p -> snd p)
        |> Array.take 10
    Array.append children bestParents

// Create a scored individual by applying the fitness function to assess the fitness of the given genes.
let score (fitnessFunction:Individual->float) (genes: Individual) : ScoredIndividual = (genes, fitnessFunction genes)

// Randomly generate a population containing the specified number of individuals, each with the specified number of genes.
let randomIndividuals (fitnessFunction:Individual->float) (numberGenes:int) (numberIndividuals:int) : Rand<Population> =
    rand {
        let! generateIndividuals = randArrayInit numberIndividuals (fun _ -> randomPermutation numberGenes)
        return generateIndividuals |> Array.map (fun genes -> score fitnessFunction genes) // Generate and return ScoredIndividuals
    }

// Generate a child by first randomly choosing two parents using tournament selection,
// cross their genes and then optionally mutate the resulting genes.
// Note: individuals have no gender and each parent is chosen independently, 
// so there is a small chance that the same individual may be choosen twice.
let procreate fitnessFunction (population: Population) : Rand<ScoredIndividual> =
    rand {
        let! p1 = tournamentSelect population
        let! p2 = tournamentSelect population
        let! crossedGenes = cross p1 p2
        let! probMutatedGenes = possiblyMutate crossedGenes
        return score fitnessFunction probMutatedGenes
    }

// Create a new generation by creating the specified number of children through procreation and then 
// applying elitism selection to create the population of the next generation
let evolveOneGeneration fitnessFunction (parentPopulation: Population) (childPopulationLimit: int) : Rand<Population> =
    rand {
        let createChild = procreate fitnessFunction parentPopulation
        let! createGeneration = randArrayInit childPopulationLimit (fun _ -> createChild)
        return elitismSelection parentPopulation createGeneration
    }

// Starting with the specified initial population, evolve generation by generation (forever).
// For each population, we determine the fitest individual from that generation and return an infinite sequence of these individuals.
// Due to elitism selection, the fitest individual in each succcessive generation should be at least as good as the previous generation. 
let evolveForever fitnessFunction (initialPopulation: Population) (childPopulationLimit: int): Rand<ScoredIndividual seq> =
    initialPopulation
    |> randSeqUnfold (fun currPopulation -> 
        let fitestIndividual = fitest currPopulation
        let nextGeneration = evolveOneGeneration fitnessFunction currPopulation childPopulationLimit
        (fitestIndividual, nextGeneration))

let Optimize fitnessFunction numberOfGenes numerOfIndividuals: ScoredIndividual seq =
    let solutions =
        rand {
            let! initialPopulation = randomIndividuals fitnessFunction numberOfGenes numerOfIndividuals
            return! evolveForever fitnessFunction initialPopulation numerOfIndividuals
        }
    let random = new System.Random()
    RandomMonad.evaluateWith random solutions