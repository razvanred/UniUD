using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Collections;


/*
 • il numero di telefono
• il codice PUK
• il credito disponibile in euro
• la lista delle telefonate effettuate
• la data dell’ultima ricarica
• il proprietario
• eventuali promozioni attive
-promozioni:
    - MINUTI ILLIMITATI, con costo di attivazione di 10 euro
- CHIAMA E RICHIAMA, con costo di attivazione di 15 euro
- DATI UNLIMITED, senza costi di attivazione
*/
namespace EsercizioSIM {

    struct date {
        int day;
        int month;
        int year;

        public date(int day, int month, int year) {
            this.day = day;
            this.month = month;
            this.year = year;
        }

        public bool hasOneYearPassed(date past, date present) {
            int daysPassed = 0;
            int monthsPassed = ((present.year - past.year) * 12) + (present.month - past.month);
            daysPassed = (present.day - past.day) + (monthsPassed * 30);
            return daysPassed >= 365 ? true : false;
        }

        public String getDateEuropeanFormat() {
            return "" + day + "/" + month + "/" + year;
        }
    }

    struct call {
        int minutes;
        String telephoneNum;

        public call(int minutes, String telephoneNum) {
            this.minutes = minutes;
            this.telephoneNum = telephoneNum;
        }

        public int getMinutes() {
            return this.minutes;
        }

        public String getTelNumber() {
            return this.telephoneNum;
        }
    }

    enum promotions {
        NOPROMOTION = 0,
        MINUTIILLIMITATI = 1,
        CHIAMAERICHIAMA = 2,
        DATIUNLIMITED = 3
    }

    class SIM {
        private String lastOperator;
        private String telNum;
        private double avaibleCredit;
        private List<call> calls;
        private date date;
        private String ownerNickname;
        private promotions promotion;

        //SIM attivata con promozione
        public SIM(String telNum, double avaibleCredit
            , date date, String ownerNickname, promotions promotion) {
            this.telNum = telNum;
            this.avaibleCredit = avaibleCredit;
            this.calls = new List<call>();
            this.date = date;
            this.ownerNickname = ownerNickname;
            this.promotion = promotion;
            this.lastOperator = "operator1";
        }

        //SIM attivata con portabilità
        public SIM(String telNum, double avaibleCredit, List<call> calls
            , date date, String ownerNickname, promotions promotion, String lastOperator) {
            this.telNum = telNum;
            this.avaibleCredit = avaibleCredit;
            this.calls = new List<call>();
            this.date = date;
            this.ownerNickname = ownerNickname;
            this.promotion = promotion;
            this.lastOperator = lastOperator;
        }

        public int phone(String telephoneNum) {
            Random r = new Random();
            int callMin = r.Next(1, 100);
            //call only if you have enought credit
            if ((avaibleCredit - (callMin * 2)) > 0) {
                call c = new call(callMin, telephoneNum);
                calls.Add(c);
                if ((promotion == promotions.NOPROMOTION) || (promotion == promotions.DATIUNLIMITED))
                    this.avaibleCredit -= (callMin * 2);
                return callMin;
            }
            return 0;
        }

        public void minutiIlliminati() {
            this.promotion = promotions.MINUTIILLIMITATI;
        }

        public void chiamaERichiama() {
            this.promotion = promotions.CHIAMAERICHIAMA;
        }

        public void datiUnlimited() {
            this.promotion = promotions.DATIUNLIMITED;
        }

        public List<call> getCalls() {
            return this.calls;
        }

        public void setPromotion(promotions promotion) {
            this.promotion = promotion;
        }

        public String getOperator() {
            return this.lastOperator;
        }

        public date getDate() {
            return this.date;
        }

        public String getTelNumber() {
            return this.telNum;
        }

        public String getOwnerNickname() {
            return this.ownerNickname;
        }

        public double getAvaibleCredit() {
            return this.avaibleCredit;
        }
    }
}
