using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

/*
 Il sistema dovrà permettere di:
A. creare una nuova SIM
B. creare una SIM che è stata “portabilizzata” (ovvero il cui numero
apparteneva ad un altro operatore telefonico e che non deve veder
modificata nessuna delle sue caratteristiche)
C. rimuovere una SIM
D. aggiungere una telefonata alla lista delle telefonate effettuate da una SIM
E. calcolare i minuti totali di conversazione di una SIM
F. conoscere la lista delle telefonate di una SIM verso un certo numero
G. conoscere l’elenco di tutte le telefonate effettuate da una SIM
H. attivare o disattivare una promozione su una SIM
I. sapere se una SIM è stata portabilizzata o se è originale
J. sapere se una SIM è ancora attiva o meno (una SIM si definisce “attiva” se
il periodo trascorso dall’ultima ricarica è inferiore all’anno
K. stampare in un file chiamato “info_NUMERO.txt” (dove NUMERO è
sostituito con il numero di cellulare della SIM) le seguenti informazioni
riguardanti una certa SIM:
o numero di telefono
o nome e cognome del proprietario
o credito residuo
o numero di chiamate effettuate
o originale/portabilizzata
o attiva/non attiva
o data ultima ricarica
     */

namespace EsercizioSIM {
    class SIMHandler {

        private List<SIM> sims;
        private String folderPath = System.IO.Directory.GetCurrentDirectory();

        public SIMHandler() {
            sims = new List<SIM>();
        }

        public SIM createSIM(String telNum, double avaibleCredit, List<call> calls,
            date date, String ownerNickname,
            promotions promotions, String Operator) {
            SIM s = null;
            //nuova sim non in portabilità
            if (Operator.Equals("operatore1")) {
                s = new SIM(telNum, avaibleCredit, date, ownerNickname, promotions);
            }//sim in portabilità
            else {
                s = new SIM(telNum, avaibleCredit, calls, date, ownerNickname, promotions, Operator);
            }
            sims.Add(s);
            return s;
        }

        public void removeSIM(SIM sim) {
            sims.Remove(sim);
        }

        public void phone(SIM sim, String telNumber) {
            sim.phone(telNumber);
        }

        public int totalMinutes(SIM sim) {
            List<call> calls = sim.getCalls();
            int totalMinutes = 0;
            foreach(call c in calls) {
                totalMinutes += c.getMinutes();
            }
            return totalMinutes;
        }

        public List<call> callsListTowardNumber(SIM sim, String telNumber) {
            List<call> calls = sim.getCalls();
            List<call> callsTowardNumber = new List<call>();
            foreach (call c in calls) {
                if (c.getTelNumber().Equals(telNumber)) {
                    callsTowardNumber.Add(c);
                }
            }
            return callsTowardNumber;
        }

        public List<call> allCalls(SIM sim) {
            return sim.getCalls();
        }

        public void changePromotionsStatus(SIM sim, promotions promotionCode) {
            sim.setPromotion(promotionCode);
        }

        public bool isPortable(SIM sim) {
            return sim.getOperator().Equals("operator1") ? false : true;
        }

        public bool isStillActive(SIM sim, date today) {
            return today.hasOneYearPassed(sim.getDate(), today);
        }

        /*
         o numero di telefono
o nome e cognome del proprietario
o credito residuo
o numero di chiamate effettuate
o originale/portabilizzata
o attiva/non attiva
o data ultima ricaric
             */
        public void registerInFile(SIM sim) {

            StreamWriter sw = new StreamWriter(folderPath + "/" + sim.getTelNumber() + ".txt");
            sw.WriteLine(sim.getOwnerNickname());
            sw.WriteLine(sim.getAvaibleCredit());
            sw.WriteLine(this.allCalls(sim).Count());
            sw.WriteLine(isPortable(sim) ? "portabilizzata" : "originale");
            sw.WriteLine(isStillActive(sim, new date(22, 10, 2019)) ? "inattiva" : "attiva");
            sw.WriteLine(sim.getDate().getDateEuropeanFormat());
            sw.Flush();
            sw.Close();
        }
    }
}
