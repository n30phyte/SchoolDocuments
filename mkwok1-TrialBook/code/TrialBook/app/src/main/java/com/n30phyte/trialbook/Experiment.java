package com.n30phyte.trialbook;

import android.os.Parcel;
import android.os.Parcelable;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * Main model for experiments, storing the description, date, trials and passes.
 * <p>
 * Implements Parcelable instead of Serializable due to having to store SimpleDateFormat inside.
 */
public class Experiment implements Parcelable {
    /**
     * Required CREATOR static member to parcel and unparcel objects for Android APIs
     */
    public static final Parcelable.Creator<Experiment> CREATOR = new Parcelable.Creator<Experiment>() {
        public Experiment createFromParcel(Parcel in) {
            return new Experiment(in);
        }

        /**
         * Unused, since an array of experiments is never passed around with the parcelable feature.
         */
        @Override
        public Experiment[] newArray(int size) {
            return new Experiment[size];
        }
    };

    private final SimpleDateFormat sdf;
    private String description;
    private Date recordedDate;
    private int fails;
    private int pass;

    /**
     * Constructor for a new experiment. Newly created experiments usually won't have any trials and such, so can be left out of the constructor parameters.
     * <p>
     * Throws an exception when the date is not in the expected format.
     *
     * @param description
     *         Description/name of the experiment
     * @param recordedDate
     *         Date that should be recorded in the experiment
     */
    public Experiment(String description, String recordedDate) throws ParseException {
        sdf = new SimpleDateFormat("yyyy-MM-dd", Locale.CANADA);

        this.description = description;

        this.recordedDate = sdf.parse(recordedDate);
        fails = 0;
        pass = 0;
    }

    /**
     * Constructor for when it needs to be passed between activities.
     */
    public Experiment(Parcel input) {
        sdf = new SimpleDateFormat("yyyy-MM-dd", Locale.CANADA);

        try {
            this.description = input.readString();
            this.setDateFromString(input.readString());
            this.fails = input.readInt();
            this.pass = input.readInt();
        } catch (ParseException e) {
            e.printStackTrace();
        }
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

    /**
     * Gets the currently saved date as a String
     *
     * @return Returns a String with the date formatted to the expected format.
     */
    public String getDateAsString() {
        return sdf.format(getRecordedDate());
    }

    /**
     * Sets the currently saved date from a string, with the pre-specified format used.
     * <p>
     * Throws an exception when the entry isn't in the expected format.
     */
    public void setDateFromString(String dateString) throws ParseException {
        setRecordedDate(sdf.parse(dateString));
    }

    public int getTrials() {
        return fails + pass;
    }

    public int getFails() {
        return fails;
    }

    public void incrementFails() {
        this.fails++;
    }

    public int getPass() {
        return pass;
    }

    public void incrementPass() {
        this.pass++;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(description);
        dest.writeString(this.getDateAsString());
        dest.writeInt(this.fails);
        dest.writeInt(this.pass);
    }
}
