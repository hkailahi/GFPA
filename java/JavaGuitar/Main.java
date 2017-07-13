import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
        int strings = 6, frets = 24;

        System.out.println("Here's your Guitar!\n");

        Guitar g = new Guitar(frets, strings);
        g.printFrets(5);
        for (int i = 6; i >0; i--) {
            System.out.println(i + " |");
        }
        System.out.println("S^");

        System.out.println("\nWe've given you 6 strings, 12 frets, and put it in Standard Tuning (EADGBE).\n");

        System.out.println("Here's your guitar with all the note labeled:\n");
        g.printGuitarAsNoteNames();

        boolean cont = true;
        int opt;

        while(cont) {
            System.out.println("\nOk what do you want to do?");
            System.out.println("\t1: View notes\n\t2: View notes in integer notation\n\t" +
                    "3: View note octaves\n\t4: Find notes in a key\n\t5: Exit\n");
            System.out.println("Enter selection: ");
            Scanner input = new Scanner(System.in);
            opt = input.nextInt();
            switch (opt) {
                case 1: {
                    g.printGuitarAsNoteNames();
                    break;
                }
                case 2: {
                    g.printGuitarAsNoteIntegers();
                    break;
                }
                case 3: {
                    g.printGuitarAsOctaves();
                    break;
                }
                case 4: {
                    Song s = new Song();
                    s.queryForSongInfo();

                    g.setAllDegrees(s.getScale());
                    s.getScale().printScaleDegrees();

                    System.out.println("\nHere are those notes, in \"" + s.getKey() + "\", on your guitar");
                    g.printGuitarAsScaleDegrees();
                    break;
                }
                case 5: {
                    System.out.println("See ya!");
                    cont = false;
                    break;
                }
                default: {
                    System.out.println("Invalid input, please choose enter either 1, 2, 3, or 4.");
                }
            }
        }
    }
}
