using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EsercizioSIM {
    class Program {
        static void Main(string[] args) {
            SIMHandler sH = new SIMHandler();
            String OPERATOR = "operator1";
            SIM s1 = sH.createSIM("3276319633", 10000, null,
                new date(10,10,2019), "tizio",
                promotions.NOPROMOTION, OPERATOR);
            SIM s2 = sH.createSIM("380438648", 10000, null,
                new date(8, 3, 2017), "caio",
                promotions.CHIAMAERICHIAMA, OPERATOR);
            SIM s3 = sH.createSIM("0438738374", 10000, null,
                new date(6, 4, 2018), "semprogno",
                promotions.MINUTIILLIMITATI, "operator2");
            sH.phone(s1, "380438648");
            sH.phone(s1, "0438738374");
            sH.phone(s2, "380438648");
            sH.phone(s3, "380438648");
            sH.phone(s3, "3276319633");
            sH.phone(s2, "3276319633");
            sH.registerInFile(s1);
            sH.registerInFile(s2);
            sH.registerInFile(s3);
        }
    }
}
