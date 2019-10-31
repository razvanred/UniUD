package ro.razvan.uniud.oop.telephoneOperator;

import org.jetbrains.annotations.NotNull;
import ro.razvan.uniud.oop.telephoneOperator.database.InMemoryDatabase;
import ro.razvan.uniud.oop.telephoneOperator.model.Person;
import ro.razvan.uniud.oop.telephoneOperator.model.PhoneCall;
import ro.razvan.uniud.oop.telephoneOperator.model.Promotion;
import ro.razvan.uniud.oop.telephoneOperator.model.simCard.SimCard;
import ro.razvan.uniud.oop.telephoneOperator.parser.SimCardJsonParser;
import ro.razvan.uniud.oop.telephoneOperator.parser.SimCardTxtParser;
import ro.razvan.uniud.oop.telephoneOperator.printer.SimCardPrinter;
import ro.razvan.uniud.oop.telephoneOperator.printer.SimCardPrinterComponent;
import ro.razvan.uniud.oop.telephoneOperator.stdout.OutputStream;
import ro.razvan.uniud.oop.telephoneOperator.stdout.Outputter;
import ro.razvan.uniud.oop.telephoneOperator.stdout.fileOutput.FileOutputComponent;
import ro.razvan.uniud.oop.telephoneOperator.telephoneOperator.TelephoneOperator;

import javax.inject.Inject;
import java.util.Arrays;
import java.util.Objects;

public final class App {

    @NotNull
    private final Outputter outputStream;

    @NotNull
    private final InMemoryDatabase database;

    @NotNull
    private final FileOutputComponent.Factory fileOutputComponentFactory;

    @NotNull
    private final SimCardPrinterComponent.Factory simCardPrinterComponentFactory;

    @Inject
    public App(
            @OutputStream @NotNull final Outputter outputStream,
            @NotNull final FileOutputComponent.Factory fileOutputComponentFactory,
            @NotNull final SimCardPrinterComponent.Factory simCardPrinterComponentFactory,
            @NotNull final InMemoryDatabase database
    ) {
        this.outputStream = outputStream;
        this.database = database;
        this.fileOutputComponentFactory = fileOutputComponentFactory;
        this.simCardPrinterComponentFactory = simCardPrinterComponentFactory;
    }

    public static void main(String[] args) {
        AppComponent.create().app();
    }

    @Inject
    public void execute() {
        final @NotNull Person stefan = new Person("Stefan", "Rosu");
        final @NotNull Person emma = new Person("Emma Violetta", "Rosu");
        final @NotNull Person razvan = new Person("Razvan", "Rosu");

        final @NotNull TelephoneOperator ho = database.createOperator("ho.");

        final @NotNull TelephoneOperator iliad = database.createOperator("iliad");

        // Create a SimCard
        final @NotNull SimCard stefanIliad = iliad.createSim(stefan);
        outputStream.output(iliad.toString());
        outputStream.output(stefanIliad.toString());

        printSeparator();

        // Port a SimCard
        final @NotNull SimCard stefanHo = ho.portSim(stefanIliad);
        outputStream.output(ho.toString());
        outputStream.output(stefanHo.toString());

        printSeparator();

        // Remove a SimCard
        iliad.removeSimCard(stefanIliad);
        outputStream.output(iliad.toString());

        printSeparator();

        // Add a PhoneCall to a specific SIM
        final @NotNull SimCard emmaIliad = iliad.createSim(emma, Promotion.UNLIMITED_MINUTES);
        stefanHo.addPhoneCall(new PhoneCall(60, emmaIliad.getTelephoneNumber()));
        outputStream.output(Arrays.toString(stefanHo.getPhoneCalls()));

        printSeparator();

        // Calculate the total amount of conversation minutes of a SIM
        final @NotNull SimCard razvanHo = ho.createSim(razvan, Promotion.CALL_AND_RECALL);
        stefanHo.addPhoneCall(new PhoneCall(60, emmaIliad.getTelephoneNumber()));
        stefanHo.addPhoneCall(new PhoneCall(50, razvanHo.getTelephoneNumber()));
        outputStream.output(Boolean.toString(stefanHo.getConversationTimeInMinutes() == 170));

        printSeparator();

        // Retrieve the list of the phone calls made to a specific number by a SIM
        outputStream.output(
                Boolean.toString(
                        Arrays.equals(
                                new PhoneCall[]{
                                        new PhoneCall(60, emmaIliad.getTelephoneNumber()),
                                        new PhoneCall(60, emmaIliad.getTelephoneNumber())
                                },
                                stefanHo.getPhoneCallsByPhoneNumber(emmaIliad.getTelephoneNumber())
                        ) && Arrays.equals(
                                new PhoneCall[]{new PhoneCall(50, razvanHo.getTelephoneNumber())},
                                stefanHo.getPhoneCallsByPhoneNumber(razvanHo.getTelephoneNumber())
                        )
                )
        );

        printSeparator();

        // Retrieve the list of the phone calls made by a SIM
        outputStream.output(Arrays.toString(stefanHo.getPhoneCalls()));

        printSeparator();

        // Activate or deactivate a promotion on a SIM
        stefanHo.changeActivePromotion(Promotion.UNLIMITED_DATA);
        razvanHo.deactivatePromotion();
        outputStream.output(Objects.requireNonNull(stefanHo.getActivePromotion()).toString());
        outputStream.output(Boolean.toString(razvanHo.getActivePromotion() == null));

        printSeparator();

        // Find out if a SIM is ported
        outputStream.output(Boolean.toString(ho.isSimPorted(stefanHo) && !ho.isSimPorted(razvanHo) && !iliad.isSimPorted(emmaIliad)));

        printSeparator();

        // Find out if a SIM is still active
        outputStream.output(Boolean.toString(stefanHo.isActive() && razvanHo.isActive() && emmaIliad.isActive()));

        printSeparator();

        // Print on a TXT file the information about the SIM
        printTxtSimCard(stefanHo, ho);
        printJsonSimCard(stefanHo, ho);
        printJsonSimCardOnOutputStream(stefanHo, ho);

    }

    private void printTxtSimCard(final @NotNull SimCard simCard, final @NotNull TelephoneOperator operator) {
        final @NotNull SimCardPrinter printer = simCardPrinterComponentFactory.create(
                new SimCardTxtParser(simCard, operator),
                fileOutputComponentFactory.create("info_" + simCard.getTelephoneNumber() + ".txt").fileOutput()
        ).printer();

        printer.print();
    }

    private void printJsonSimCard(final @NotNull SimCard simCard, final @NotNull TelephoneOperator operator) {
        final @NotNull SimCardPrinter printer = simCardPrinterComponentFactory.create(
                new SimCardJsonParser(simCard, operator),
                fileOutputComponentFactory.create("info_" + simCard.getTelephoneNumber() + ".json").fileOutput()
        ).printer();

        printer.print();
    }

    private void printJsonSimCardOnOutputStream(final @NotNull SimCard simCard, final @NotNull TelephoneOperator operator) {
        final @NotNull SimCardPrinter printer = simCardPrinterComponentFactory.create(
                new SimCardJsonParser(simCard, operator),
                outputStream
        ).printer();

        printer.print();
    }

    private void printSeparator() {
        outputStream.output(" ----- ");
    }
}
