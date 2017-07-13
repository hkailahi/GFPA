import java.util.Scanner;

public class Song {

    private Scale scale;
    private String key;

    public Song() {}

    public Scale getScale() {
        return scale;
    }

    public String getKey() {
        return key;
    }

    public void queryForSongInfo() {
        Scanner input = new Scanner(System.in);

        System.out.println("Choose a Key Signature:\n\tMajor options: C, G, D, A, E, B, F#, Db, Ab, Eb, Bb, F" +
                "\n\tMinor options: Am, Em, Bm, F#m, C#m, Abm, Ebm, Bbm, Fm, Cm, Gm, Dm");
        System.out.println("\nEnter key (case-sensitive): ");

        key = input.nextLine();
        while (!key.matches("(?![EB]#)(?![FC]b)([A-G])([#b])?([m])?")) {
            System.out.println("You have entered an invalid key. Please enter one from either a major key " +
                    "(C, G, D, A, E, B, F#, Db, Ab, Eb, Bb, F)" + " or one from a minor key " +
                    "(Am, Em, Bm, F#m, C#m, Abm, Ebm, Bbm, Fm, Cm, Gm, Dm).");
            System.out.println("Enter Key Signature:");
            key = input.nextLine();
        }

        this.scale = new Scale();
        scale.setKeySig( scale.getKeySigAsInt(key) );
        System.out.print("Key: " + key );
        scale.printKeySignature();
    }

}
