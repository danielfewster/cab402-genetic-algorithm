using System;
using System.Collections.Generic;
using Microsoft.FSharp.Core;

namespace CAB402.CSharp
{
    public class GeneticAlgorithm
    {
        private class Individual // Class represents an Individual that has an array of Genes
        {
            private int[] genes;

            public Individual(int[] genes) { this.genes = genes; }

            public int[] Genes { get => genes; }
        }

        private class ScoredIndividual // Class represents a ScoredIndividual that has Genes, and a Fitness Score
        {
            public Tuple<int[], double> scoredIndividual;

            public ScoredIndividual(int[] individual, double fitness)
            {
                scoredIndividual = new Tuple<int[], double>(individual, fitness);
            }

            public Individual Individual { get => new Individual(scoredIndividual.Item1); }

            public double Fitness { get => scoredIndividual.Item2; }
        }

        private class Population // Class that represents a Population that holds an array of ScoredIndividuals
        {
            private ScoredIndividual[] scoredIndividuals;

            public Population(ScoredIndividual[] scoredIndividuals) { this.scoredIndividuals = scoredIndividuals; }

            public ScoredIndividual[] ScoredIndividuals
            {
                set => scoredIndividuals = value;
                get => scoredIndividuals;
            }
        }

        private static Random rand = new Random(); // Global random for performance

        private static void SortDescending(ScoredIndividual[] si) // Helper Function 
        {
            ScoredIndividual temp;
            for (int i = 0; i < si.Length; i++)
                for (int j = i + 1; j < si.Length; j++)
                    if (si[i].Fitness < si[j].Fitness)
                    {
                        temp = si[i];
                        si[i] = si[j];
                        si[j] = temp;
                    }
        }

        private static ScoredIndividual Fitest(Population population)
        {
            var scoredIndividuals = population.ScoredIndividuals;
            SortDescending(scoredIndividuals);
            return scoredIndividuals[0];
        }

        private static Individual TournamentWinner(Population competitors)
        {
            return Fitest(competitors).Individual;
        }

        private static Individual TournamentSelect(Population population)
        {
            var n1 = population.ScoredIndividuals[rand.Next(population.ScoredIndividuals.Length)];
            var n2 = population.ScoredIndividuals[rand.Next(population.ScoredIndividuals.Length)];
            return TournamentWinner(new Population(new ScoredIndividual[] { n1, n2 }));
        }

        private static Individual CrossAt(Individual parent1, Individual parent2, int splitPoint)
        {
            // Get left + parent2 genes
            var temp = new int[splitPoint + parent2.Genes.Length];
            for (int i = 0; i < splitPoint; i++)
                temp[i] = parent1.Genes[i];

            for (int i = 0; i < parent2.Genes.Length; i++)
                temp[i + splitPoint] = parent2.Genes[i];

            // Make distinct
            var crossover = new int[parent2.Genes.Length];
            int idx = 0;
            var alreadyIn = new Dictionary<int, bool>();
            for (int i = 0; i < temp.Length; i++)
                if (!alreadyIn.ContainsKey(temp[i]))
                {
                    crossover[idx++] = temp[i];
                    alreadyIn.Add(temp[i], true);
                }

            return new Individual(crossover);
        }

        private static Individual Cross(Individual parent1, Individual parent2)
        {
            var index = rand.Next(1, parent1.Genes.Length - 1);
            return CrossAt(parent1, parent2, index);
        }

        private static Individual ReverseMutateAt(Individual genes, int firstIndex, int secondIndex) 
        {
            var revGenes = genes.Genes;
            Array.Reverse(revGenes);

            var newGenes = new int[genes.Genes.Length];
            for (int i = 0; i < newGenes.Length; i++)
                if (i >= firstIndex && i <= secondIndex) // if in range of middle, get a reversed gene
                    newGenes[i] = revGenes[i];
                else
                    newGenes[i] = genes.Genes[i]; // otherwise, get normal gene

            return new Individual(newGenes);
        }

        private static Individual ReverseMutate(Individual chromosome) 
        {
            var first = rand.Next(chromosome.Genes.Length - 1);
            var second = rand.Next(first + 1, chromosome.Genes.Length);
            return ReverseMutateAt(chromosome, first, second);
        }

        private static double mutateProbability = 0.15;

        private static bool GetProbability()
        {
            return rand.NextDouble() <= mutateProbability;
        }

        private static Individual PossiblyMutate(Individual genes) 
        {
            return GetProbability() ? ReverseMutate(genes) : genes;
        }

        private static Population ElitismSelection(Population parents, Population children) 
        {
            var scoredIndividuals = parents.ScoredIndividuals;
            SortDescending(scoredIndividuals);

            var newPopulation = new ScoredIndividual[children.ScoredIndividuals.Length + 10];
            children.ScoredIndividuals.CopyTo(newPopulation, 0);

            for (int i = 0; i < 10; i++)
                newPopulation[i + children.ScoredIndividuals.Length] = scoredIndividuals[i];

            return new Population(newPopulation);
        }

        private static ScoredIndividual Score(FSharpFunc<int[], double> fitnessFunction, Individual genes) 
        {
            return new ScoredIndividual(genes.Genes, fitnessFunction.Invoke(genes.Genes));
        }

        private static void Shuffle(int[] arr) // Fisher-Yates shuffle
        {
            for (int i = arr.Length - 1; i > 0; i--)
            {
                int swapIndex = rand.Next(i + 1);
                int tmp = arr[swapIndex];
                arr[swapIndex] = arr[i];
                arr[i] = tmp;
            }
        }

        private static int[] RandomPermutation(int n)
        {
            int[] genes = new int[n];
            for (int i = 0; i < genes.Length; i++)
                genes[i] = i;

            Shuffle(genes);

            return genes;
        }

        private static Population RandomIndividuals(FSharpFunc<int[], double> fitnessFunction, int numberGenes, int numberIndividuals) 
        {
            var generateIndividuals = new Individual[numberIndividuals]; // Generate Individuals
            for (int i = 0; i < numberIndividuals; i++)
                generateIndividuals[i] = new Individual(RandomPermutation(numberGenes));

            var generateScoredIndividuals = new ScoredIndividual[numberIndividuals]; // Score Individuals
            for (int i = 0; i < generateScoredIndividuals.Length; i++)
                generateScoredIndividuals[i] = Score(fitnessFunction, generateIndividuals[i]);

            return new Population(generateScoredIndividuals);
        }

        private static ScoredIndividual Procreate(FSharpFunc<int[], double> fitnessFunction, Population population) 
        {
            var p1 = TournamentSelect(population);
            var p2 = TournamentSelect(population);
            var crossedGenes = Cross(p1, p2);
            return Score(fitnessFunction, PossiblyMutate(crossedGenes));
        }

        private static Population EvolveOneGeneration(FSharpFunc<int[], double> fitnessFunction, Population parentPopulation, int childPopulationLimit)
        {
            var createGeneration = new ScoredIndividual[childPopulationLimit];
            for (int i = 0; i < childPopulationLimit; i++)
                createGeneration[i] = Procreate(fitnessFunction, parentPopulation);
            return ElitismSelection(parentPopulation, new Population(createGeneration));
        }

        private static IEnumerable<Tuple<int[], double>> EvolveForever(FSharpFunc<int[], double> fitnessFunction, Population initialPopulation, int childPopulationLimit)
        {
            var nextGeneration = initialPopulation;
            while (true) 
            {
                var fitestIndividual = Fitest(nextGeneration);
                yield return new Tuple<int[], double>(fitestIndividual.Individual.Genes, fitestIndividual.Fitness);
                nextGeneration = EvolveOneGeneration(fitnessFunction, nextGeneration, childPopulationLimit);
            }
        }

        public static IEnumerable<Tuple<int[],double>> Optimize(FSharpFunc<int[], double> fitnessFunction, int numberOfGenes, int numerOfIndividuals)
        {
            var initialPopulation = RandomIndividuals(fitnessFunction, numberOfGenes, numerOfIndividuals);
            return EvolveForever(fitnessFunction, initialPopulation, numerOfIndividuals);
        }
    }
}
