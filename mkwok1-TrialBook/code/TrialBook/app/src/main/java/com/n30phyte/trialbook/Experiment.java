package com.n30phyte.trialbook;

import java.io.Serializable;
import java.util.Date;

public class Experiment implements Serializable {
    private String description;
    private Date recordedDate;
    private int trials;
    private int pass;


    public Experiment(String description, Date recordedDate) {
        this.description = description;
        this.recordedDate = recordedDate;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String newDescription) {
        description = newDescription;
    }

    public Date getRecordedDate() {
        return recordedDate;
    }

    public void setRecordedDate(Date recordedDate) {
        this.recordedDate = recordedDate;
    }

    public int getTrials() {
        return trials;
    }

    public void incrementTrials() {
        this.trials++;
    }

    public int getPass() {
        return pass;
    }

    public void incrementPass() {
        this.pass++;
    }
}
