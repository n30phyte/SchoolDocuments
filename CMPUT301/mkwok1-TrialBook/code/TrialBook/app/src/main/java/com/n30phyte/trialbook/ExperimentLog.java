package com.n30phyte.trialbook;

import java.util.ArrayList;

/**
 * Adapted from GoF pp 128-134
 * Singleton as we would like to be able to add and remove from different Activities
 */
public class ExperimentLog {

    private static final ArrayList<Experiment> experiments = new ArrayList<>();

    /**
     * Disable construction from outside, so it's set to private.
     */
    private ExperimentLog() {
    }

    public static ArrayList<Experiment> getInstance() {
        return experiments;
    }

    public static void addExperiment(Experiment newExperiment) {
        experiments.add(newExperiment);
    }

    public static void setExperiment(int index, Experiment experiment) {
        experiments.set(index, experiment);
    }
}
