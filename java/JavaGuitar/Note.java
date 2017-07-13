public class Note implements Comparable<Note> {

    private static String[] pitchClassOpt = {"C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"};
    private int octave;
    private int note;
    private String pitchClass;
    private String degree;

    public Note(int note, int octave) {
        this.octave = octave;
        this.note = note;
        this.pitchClass = pitchClassOpt[note];

        this.degree = null; // Calculated after key signature is selected
    }

    @Override
    public int compareTo(Note other) {
        // compareTo should return < 0 if this is supposed to be
        // less than other, > 0 if this is supposed to be greater than
        // other and 0 if they are supposed to be equal
        int n1 = this.getNote() + (12 * this.getOctave());
        int n2 = other.getNote() + (12 * other.getOctave());

        return Integer.compare(n1, n2);
    }

    public int getNote() {
        return this.note;
    }

    public int getOctave() {
        return this.octave;
    }

    public String getPitchClass() {
        return pitchClass;
    }

    public String getDegree() {
        return degree;
    }

    public void setDegree(String degree) {
        this.degree = degree;
    }

}
