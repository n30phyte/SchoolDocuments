package com.n30phyte.trialbook;

import java.util.Date;

public class Experiment {
    private String Description;
    private Date RecordedDate;

    public Experiment(String description, Date recordedDate) {
        Description = description;
        RecordedDate = recordedDate;
    }

    public String getName() {
        return Description;
    }

    public void setName(String newName) {
        Description = newName;
    }

    public Date getRecordedDate() {
        return RecordedDate;
    }

    public void setRecordedDate(Date recordedDate) {
        RecordedDate = recordedDate;
    }
}
