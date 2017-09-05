package main

import (
	"fmt"
	"math"
	"math/rand"
)

func Logistic(z float64) float64 {
	return 1 / (1 + math.Exp(-z))
}

func BernoulliVariance(p float64) float64 {
	return p * (1 - p)
}

func LogisticDerivative(z float64) float64 {
	return BernoulliVariance(Logistic(z))
}

func Square(x float64) float64 {
	return x * x
}

func Error(expected float64, estimated float64) float64 {
	return 0.5 * Square(expected-estimated)
}

func ErrorMap(expected []float64, estimated []float64) []float64 {
	errors := make([]float64, len(expected))
	for i := range expected {
		errors[i] = Error(expected[i], estimated[i])
	}
	return errors
}

func TotalError(expected []float64, estimated []float64) float64 {
	errors := ErrorMap(expected, estimated)
	total := 0.0
	for _, error := range errors {
		total += error
	}
	return total
}

type Neuron struct {
	Bias    float64
	Weights []float64
	Output  float64
	Delta   float64
}

func MakeNeuron(inputs int) *Neuron {
	output := 0.0
	bias := rand.Float64()
	weights := make([]float64, inputs)
	delta := 0.0

	for i := range weights {
		weights[i] = rand.Float64()
	}

	return &Neuron{bias, weights, output, delta}
}

func (n *Neuron) Eval(input []float64) float64 {
	n.Output = n.Bias
	for i, weight := range n.Weights {
		n.Output += weight * input[i]
	}
	n.Output = Logistic(n.Output)

	return n.Output
}

type Layer struct {
	Neurons []*Neuron
	Results []float64
	Deltas  []float64
}

func MakeLayer(inputs, outputs int) *Layer {
	neurons := make([]*Neuron, outputs)
	results := make([]float64, outputs)
	deltas := make([]float64, outputs)

	for i := range neurons {
		neurons[i] = MakeNeuron(inputs)
	}

	return &Layer{neurons, results, deltas}
}

func (l *Layer) Eval(input []float64) []float64 {
	for i, neuron := range l.Neurons {
		l.Results[i] = neuron.Eval(input)
	}
	return l.Results
}

type Network struct {
	inputs int
	Hidden *Layer
	Output *Layer
}

func MakeNetwork(inputSize, hiddenSize, outputSize int) *Network {
	hidden := MakeLayer(inputSize, hiddenSize)
	output := MakeLayer(hiddenSize, outputSize)

	return &Network{inputSize, hidden, output}
}

func (n *Network) Predict(input []float64) []float64 {
	return n.Output.Eval(n.Hidden.Eval(input))
}

func (n *Network) Train(input []float64, target []float64) {
	n.Predict(input)

	// Compute the deltas for the output layer
	outputResults := n.Output.Results
	for i, outputResult := range outputResults {
		delta := (outputResult - target[i]) * BernoulliVariance(outputResult)
		n.Output.Neurons[i].Delta = delta
		n.Output.Deltas[i] = delta
	}

	// Compute the deltas for the hidden layer
	hiddenResults := n.Hidden.Results
	for i, hiddenResult := range hiddenResults {
		delta := 0.0
		for _, neuron := range n.Output.Neurons {
			delta += neuron.Delta * neuron.Weights[i]
		}
		delta = delta * BernoulliVariance(hiddenResult)
		n.Hidden.Neurons[i].Delta = delta
		n.Hidden.Deltas[i] = delta
	}

	learningRate := 0.5

	// Update weights
	for i := range input {
		for _, neuron := range n.Hidden.Neurons {
			neuron.Weights[i] -= learningRate * neuron.Delta * input[i]
		}
	}
	for i := range n.Hidden.Neurons {
		for _, neuron := range n.Output.Neurons {
			neuron.Weights[i] -= learningRate * neuron.Delta * n.Hidden.Neurons[i].Output
		}
	}

	// Update biases
	for _, neuron := range n.Hidden.Neurons {
		neuron.Bias -= learningRate * neuron.Delta
	}

	for _, neuron := range n.Output.Neurons {
		neuron.Bias -= learningRate * neuron.Delta
	}
}

func main() {

	nn := MakeNetwork(2, 2, 2)
	nn.Hidden.Neurons[0].Weights[0] = 0.15
	nn.Hidden.Neurons[0].Weights[1] = 0.20
	nn.Hidden.Neurons[0].Bias = 0.35
	nn.Hidden.Neurons[1].Weights[0] = 0.25
	nn.Hidden.Neurons[1].Weights[1] = 0.30
	nn.Hidden.Neurons[1].Bias = 0.35
	nn.Output.Neurons[0].Weights[0] = 0.40
	nn.Output.Neurons[0].Weights[1] = 0.45
	nn.Output.Neurons[0].Bias = 0.60
	nn.Output.Neurons[1].Weights[0] = 0.50
	nn.Output.Neurons[1].Weights[1] = 0.55
	nn.Output.Neurons[1].Bias = 0.60

	prediction := nn.Predict([]float64{0.05, 0.10})
	fmt.Println("Initial prediction:", prediction)

	errors := ErrorMap([]float64{0.01, 0.99}, prediction)
	fmt.Println("Output error:", errors)
	totalError := TotalError([]float64{0.01, 0.99}, prediction)
	fmt.Println("Total error:", totalError)
	nn.Train([]float64{0.05, 0.10}, []float64{0.01, 0.99})

	/*
		fmt.Println("new w5:", nn.Output.Neurons[0].Weights[0])
		fmt.Println("new w6:", nn.Output.Neurons[0].Weights[1])
		fmt.Println("new b3:", nn.Output.Neurons[0].Bias)
		fmt.Println("new w7:", nn.Output.Neurons[1].Weights[0])
		fmt.Println("new w8:", nn.Output.Neurons[1].Weights[1])
		fmt.Println("new b4:", nn.Output.Neurons[1].Bias)
		fmt.Println("new w1:", nn.Hidden.Neurons[0].Weights[0])
		fmt.Println("new w2:", nn.Hidden.Neurons[0].Weights[1])
		fmt.Println("new b1:", nn.Hidden.Neurons[0].Bias)
		fmt.Println("new w3:", nn.Hidden.Neurons[1].Weights[0])
		fmt.Println("new w4:", nn.Hidden.Neurons[1].Weights[1])
		fmt.Println("new b2:", nn.Hidden.Neurons[1].Bias)
	*/

	prediction = nn.Predict([]float64{0.05, 0.10})
	fmt.Println("New prediction:", prediction)
	fmt.Println("New total error:", TotalError([]float64{0.01, 0.99}, prediction))

	fmt.Println("Iterating with the same values")
	for i := 0; i < 10000; i++ {
		nn.Train([]float64{0.05, 0.10}, []float64{0.01, 0.99})
	}
	prediction = nn.Predict([]float64{0.05, 0.10})
	fmt.Println("Final prediction:", prediction)
	fmt.Println("Final total error:", TotalError([]float64{0.01, 0.99}, prediction))
}
