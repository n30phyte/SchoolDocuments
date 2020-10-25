import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.util.Calendar;

/**
 * Assignment 1: Using standard libraries <br />
 * Calculate age in days
 */
public class DaysOld {

    /**
     * Calculate how many days between today and the date, and them out
     *
     * @param birthday {@code String} The start date
     */
    public static void days(String birthday) {

        // Formatter to take in input.
        var inputFormat = new SimpleDateFormat("yyyy-MM-dd");

        // Try to parse the string, returning from function if invalid string.
        try {
            inputFormat.parse(birthday);
        } catch (ParseException pe) {
            System.out.println("Incorrect format!");
            return;
        }

        // Convert the parsed value into a Calendar object
        var birthdayDate = inputFormat.getCalendar();
        // Get today as a Calendar object
        var today = Calendar.getInstance();

        // Format for the output values.
        var outputFormat = new SimpleDateFormat("MMMM dd yyyy");

        // Format outputs as string.
        var formattedBirthday = outputFormat.format(birthdayDate.getTime());
        var formattedToday = outputFormat.format(today.getTime());

        // Print out invariant message.
        System.out.format("Birthday: %s; today: %s -- ", formattedBirthday, formattedToday);

        // Compare dates and print out appropriate message for each situation.
        // obj1.compareTo(obj2) returns a value < 0 if obj2 is after obj1.
        if (today.compareTo(birthdayDate) >= 0) {
            // Calculate days with built in Java function.
            var days = Duration.between(birthdayDate.toInstant(), today.toInstant()).toDays();

            System.out.format("You are %d days old.%n", days);
        } else {
            System.out.println("Wrong birthday!");
        }
    }

    /**
     * Main entry
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {
        days("2000-1-1");
        days("3000-1-1");           // This is a wrong birthday
        // Add your birthday
        days("2000-01-02");
    }

}
