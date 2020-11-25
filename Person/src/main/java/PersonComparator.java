import java.util.ArrayList;
import java.util.Scanner;

public class PersonComparator implements java.util.Comparator<Person> {

    public int compare(Person left, Person right) {
        if (left.getAge() > right.getAge()) {
            return 1;
        } else if (left.getAge() < right.getAge()) {
            return -1;
        } else {
            return 0;
        }
    }


    public static void main(String[] args) {
        var input = new Scanner(System.in);
        //Declare and create an array
        var scores = new ArrayList<Double>();
        //Initialize sum accumulator to 0
        var sum = 0.0;
        double scoreIn = 0;
        //If score is negative, end the while loop
        //Otherwise populate the array
        while (scoreIn >= 0) {
            System.out.print("Enter a new score or a negative number to exit: ");
            scoreIn = input.nextDouble();

            //Accumulate total scores
            if (scoreIn < 0) {
                break;
            } else {
                scores.add(scoreIn);
                sum += scoreIn;
            }
        }
        //Output total scares entered
        System.out.println("Total scores entered: " + scores.size());
        //Calculate average score
        double average = sum / scores.size();
        int scoresAbove = 0;
        int scoresBelow = 0;
        //Traverse the array backwards
        for (var score : scores) {
            //If the score is >= average
            //Yes - increment the above counter
            //No - increment the below counter
            if (score >= average) {
                scoresAbove++;
            } else {
                scoresBelow++;
            }
        }
        //When done looping, pout out totals
        System.out.println("Average score: " + average);
        System.out.println("Number of scores above or equal to the average: " + scoresAbove);
        System.out.println("Number of scores below the average: " + scoresBelow);
    }
}

