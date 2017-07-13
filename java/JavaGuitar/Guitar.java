import java.util.ArrayList;
import java.util.List;

public class Guitar {

    private Note[][] guitar;
    private int numFrets;
    private int numStrings;

    public Guitar(int numFrets, int numStrings) {
        this.numFrets = numFrets;
        this.numStrings = numStrings;

        initGuitar();
    }

    public void initGuitar() {
        this.guitar = new Note[numFrets][numStrings];

        // Assume standard tuning for initial implementation
        setTuning("standard");

        // Initialize notes -> guitar[fret][string]
        for (int string = 0; string < numStrings; string++ ) { // column
            for (int fret = 1; fret < numFrets; fret++) { // row
                Note prev = guitar[fret-1][string];
                int currNote = (prev.getNote()+1)%12;
                int currOctave = prev.getOctave();
                if (currNote==0) {
                    currOctave++; // "C" is the start of an octave in Hemholtz system
                }
                guitar[fret][string] = new Note(currNote, currOctave);
            }
        }
    }

    public void setAllDegrees(Scale s) {

        // Initialize degrees -> guitar[fret][string]
        for (int string = 0; string < numStrings; string++ ) { // column
            for (int fret = 0; fret < numFrets; fret++) { // row
                guitar[fret][string].setDegree(s.calcNoteDegree(guitar[fret][string]));
            }
        }
    }

    public void setTuning(String tuning) {
        if (tuning=="standard") {
            guitar[0][0] = new Note(4, 2);   // E2
            guitar[0][1] = new Note(9, 2);  // A2
            guitar[0][2] = new Note(2, 3);  // D3
            guitar[0][3] = new Note(7, 3);  // G3
            guitar[0][4] = new Note(11, 3); // B3
            guitar[0][5] = new Note(4, 4);  // E4

        } else {
            throw new RuntimeException("Non-standard tunings unavailable");
            // todo implement other tunings
        }
    }

    /*====================  Printing Methods ====================*/

    public void printFrets(int spacing) {
        StringBuilder sb = new StringBuilder();
        List<Integer> args = new ArrayList<>();

        sb.append("F> ");
        for (int i = 0; i<=12; i++) {
            sb.append("%" + spacing + "s");
            args.add(i);
        }

        String s = String.format(sb.toString(), args.toArray());
        int l = s.length();
        String n = "-";

        System.out.println(s);
        s = new String(new char[l]).replace("\0", n);
        System.out.println(s);

    }

    public void printGuitarAsNoteIntegers() {
        int spacing = 5;

        printFrets(spacing);

        StringBuilder sb = new StringBuilder();
        List<Integer> args = new ArrayList<>();

        for (int string = 5; string >= 0; string-- ) {
            sb.append(string+1);
            sb.append(" |");
            for (int fret = 0; fret <= 12; fret++) {
                sb.append("%" + spacing + "s");
                args.add(guitar[fret][string].getNote());
            }
            sb.append("\n");
        }
        System.out.print(String.format(sb.toString(), args.toArray()));
        System.out.println("S^");
    }

    public void printGuitarAsNoteNames() {
        int spacing = 10;

        printFrets(spacing);

        StringBuilder sb = new StringBuilder();
        List<String> args = new ArrayList<>();

        for (int string = 5; string >= 0; string-- ) {
            sb.append(string+1);
            sb.append(" |");
            for (int fret = 0; fret <= 12; fret++) {
                sb.append("%" + spacing + "s");
                args.add(guitar[fret][string].getPitchClass());

            }
            sb.append("\n");
        }

        System.out.print(String.format(sb.toString(), args.toArray()));
        System.out.println("S^");
    }

    public void printGuitarAsOctaves() {
        int spacing = 5;

        printFrets(spacing);

        StringBuilder sb = new StringBuilder();
        List<Integer> args = new ArrayList<>();

        for (int string = 5; string >= 0; string-- ) {
            sb.append(string+1);
            sb.append(" |");
            for (int fret = 0; fret <= 12; fret++) {
                sb.append("%" + spacing + "s");
                args.add(guitar[fret][string].getOctave());

            }
            sb.append("\n");
        }

        System.out.print(String.format(sb.toString(), args.toArray()));
        System.out.println("S^");
    }

    public void printGuitarAsScaleDegrees() {
        int spacing = 5;

        printFrets(spacing);

        StringBuilder sb = new StringBuilder();
        List<String> args = new ArrayList<>();

        for (int string = 5; string >= 0; string-- ) {
            sb.append(string+1);
            sb.append(" |");
            for (int fret = 0; fret <= 12; fret++) {
                sb.append("%" + spacing + "s");
                args.add(guitar[fret][string].getDegree());
            }
            sb.append("\n");
        }
        System.out.print(String.format(sb.toString(), args.toArray()));
        System.out.println("S^");
    }

}
