public class Scale {

    private final String[] majorSignatures = {"C", "G", "D", "A", "E", "B", "F#", "Db", "Ab", "Eb", "Bb", "F"}; // omitting enharmonic cb, c#, gb
    private final String[] minorSignatures = {"Am", "Em", "Bm", "F#m", "C#m", "Abm", "Ebm", "Bbm", "Fm", "Cm", "Gm", "Dm"}; // omitting enharmonic g#, d#, a#
    private final String[] chromaticSharpFive = {"1", "b2", "2", "b3", "3", "4", "b5", "5", "#5", "6", "b7", "7"};
    private final String[] chromaticFlatSix = {"1", "b2", "2", "b3", "3", "4", "b5", "5", "b6", "6", "b7", "7"};
    private final int[] majorDegrees = {0, 2, 4, 5, 7, 9, 11};
    private final int[] minorDegrees = {0, 2, 3, 5, 7, 8, 10};

    private int keySig;
    private boolean isMajor = false;

    public Scale() {}

    public void printScaleDegrees() {
        if (isMajor) {
            System.out.print("The notes of a major key signature are the ");
            for (int i = 0; i < 6; i++) {
                System.out.print(chromaticSharpFive[majorDegrees[i]] + ", ");
            }
            System.out.println("and the " + chromaticSharpFive[majorDegrees[6]] + ".");
        } else {
            System.out.print("The notes of a minor key signature are the ");
            for (int i = 0; i < 6; i++) {
                System.out.print(chromaticFlatSix[minorDegrees[i]] + ", ");
            }
            System.out.println("and the " + chromaticFlatSix[minorDegrees[6]] + ".");
        }
    }

    public String calcNoteDegree(Note n) {
        int degree = (n.getNote()-keySig);
        if (degree < 0) {
            degree+=12;
        } else {
            degree = (degree % 12);
        }

        boolean isInScale = false;

        if (isMajor) {
            for(int i=0; i<7; i++) {
                if (majorDegrees[i]==degree) {
                    isInScale=true;
                    break;
                }
            }
        } else {
            for(int i=0; i<7; i++) {
                if (minorDegrees[i]==degree) {
                    isInScale=true;
                    break;
                }
            }
        }
        if(isInScale) {
            return isMajor ? chromaticSharpFive[degree] : chromaticFlatSix[degree];
        } else {
            return " ";
        }
    }

    // Assume input is of the form note letter + (optional) accidental + (optional) minor signifier
    public int getKeySigAsInt(String key) {
        int ks = 0;

        if (key.indexOf('m')==-1) {
            isMajor=true;
            for(int i=0; i<12; i++) {
                if (key.equals(majorSignatures[i])) {
                    break;
                }
                ks=(ks+7)%12; // Calculates position relative to C Major moving clockwise around circle of fifths
            }
        }
        else {
            isMajor=false;
            for(int i=0; i<12; i++) {
                if (key.equals(minorSignatures[i])) {
                    break;
                }
                ks=(ks+7)%12;
            }
            ks=(ks+9)%12; // Calculates position relative to A Minor moving clockwise around circle of fifths
        }
        return ks;
    }

    public void printKeySignature() {
        System.out.println(" (int notation: " + keySig + ")");
    }

    public void setKeySig(int keySig) {
        this.keySig = keySig;
    }
}
