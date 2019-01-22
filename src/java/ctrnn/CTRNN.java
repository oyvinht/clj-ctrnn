package ctrnn;

public class CTRNN implements Cloneable {

    private double[] activations;
    private double[] biases;
    private double[] externalCurrents;
    private double[] invTimeConstants;
    private double[] potentials;
    private int size;
    private double stepsize;
    private double[][] weights;
    
    public CTRNN (int size, double stepsize) {
	activations = new double[size];
	biases = new double[size];
	externalCurrents = new double[size];
	potentials = new double[size];
	this.size = size;
	this.stepsize = stepsize;
	invTimeConstants = new double[size];
	weights = new double[size][size];
	for (int i = 0; i < size; i++) {
	    activations[i] = sigmoid(potentials[i] + biases[i]);
	}
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
	CTRNN clone = (CTRNN)super.clone();
	clone.activations = this.activations.clone();
	clone.biases = this.biases.clone();
	clone.externalCurrents = this.externalCurrents.clone();
	clone.invTimeConstants = this.invTimeConstants.clone();
	clone.potentials = this.potentials.clone();
	clone.weights = this.weights.clone();
	return clone;
    }
    
    public double getActivation(int index) {
	return activations[index];
    }
    
    public double getBias(int index) {
	return biases[index];
    }

    public double getExternalCurrent(int index) {
	return externalCurrents[index];
    }

    public double getPotential(int index) {
	return potentials[index];
    }

    public double getTimeConstant(int index) {
	return 1 / invTimeConstants[index];
    }

    public double getWeight(int fromIndex, int toIndex) {
	return weights[fromIndex][toIndex];
    }
    
    public void setBias(int index, double value) {
	biases[index] = value;
    }

    public void setExternalCurrent(int index, double value) {
	externalCurrents[index] = value;
    }

    public void setPotential(int index, double value) {
	potentials[index] = value;
    }

    public void setTimeConstant(int index, double value) {
	invTimeConstants[index] = 1 / value;
    }
    
    public void setWeight(int fromIndex, int toIndex, double value) {
	weights[fromIndex][toIndex] = value;
    }

    public double sigmoid(double in) {
	return 1 / (1 + java.lang.Math.exp(-in));
    }

    public void updatePotentialsRK4() {
	int from,to;
	double input;
	double[] k1 = new double[size];
	double[] k2 = new double[size];
	double[] k3 = new double[size];
	double[] k4 = new double[size];
	double[] tmpAct = new double[size];
	double[] tmpPot = new double[size];
	// Step 1
	for (to = 0; to < size; to++) {
	    input = externalCurrents[to];
	    for (from = 0; from < size; from++) {
		input += weights[from][to] * activations[from];
	    }
	    k1[to] = stepsize * invTimeConstants[to] * (input - potentials[to]);
	    tmpPot[to] = potentials[to] + (0.5 * k1[to]);
	    tmpAct[to] = sigmoid(tmpPot[to] + biases[to]);
	}
	// Step 2
	for (to = 0; to < size; to++) {
	    input = externalCurrents[to];
	    for (from = 0; from < size; from++) {
		input += weights[from][to] * tmpAct[from];
	    }
	    k2[to] = stepsize * invTimeConstants[to] * (input - tmpPot[to]);
	    tmpPot[to] = potentials[to] + (0.5 * k2[to]);
	}
	for (to = 0; to < size; to++) {
	    tmpAct[to] = sigmoid(tmpPot[to] + biases[to]);
	}
	// Step 3
	for (to = 0; to < size; to++) {
	    input = externalCurrents[to];
	    for (from = 0; from < size; from++) {
		input += weights[from][to] * tmpAct[from];
	    }
	    k3[to] = stepsize * invTimeConstants[to] * (input - tmpPot[to]);
	    tmpPot[to] = potentials[to] + k3[to];
	}
	for (to = 0; to < size; to++) {
	    tmpAct[to] = sigmoid(tmpPot[to] + biases[to]);
	}
	// Step 4
	for (to = 0; to < size; to++) {
	    input = externalCurrents[to];
	    for (from = 0; from < size; from++) {
		input += weights[from][to] * tmpAct[from];
	    }
	    k4[to] = stepsize * invTimeConstants[to] * (input - tmpPot[to]);
	    potentials[to] +=
		(k1[to] + (2 * k2[to]) + (2 * k3[to]) + k4[to]) / 6;
	    activations[to] = sigmoid(potentials[to] + biases[to]);
	}
    }
}
