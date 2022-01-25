import java.util.regex.Pattern;

/**
 * ECE 325 - Fall 2020 <br/>
 * Assignment 2: Java regular expressions <br/>
 * Test cookies using regular expressions
 * <p>
 *
 * @author Michael Kwok
 */
public class TestCookies {

    /**
     * Verify a cookie and return the verification result
     *
     * @param cookie The cookie string
     * @return True for a legal cookie; false for an illegal one
     */
    public static boolean verifyCookie(String cookie) {
        // Capture first part (Set-Cookie until name=value)
        var tokenExpr = "[^()<>@,;: /\\[\\]?={}\\x09\\x00-\\x1F\\x7f]+";
        var octetExpr = "[\\x21\\x23-\\x2B\\x2D-\\x3A\\x3C-\\x5B\\x5D-\\x7E]+";
        var halfExpr = Pattern.compile("^(?:Set-Cookie:) (?:" + tokenExpr + "=)(?:" + octetExpr + "|\"" + octetExpr + "\")?");
        var halfMatches = halfExpr.matcher(cookie);

        if (halfMatches.matches()) {
            // Simple match. Definitely true
            return true;
        } else if (halfMatches.find()) {
            // If found as part of cookie
            if (halfMatches.start() == 0) {
                cookie = halfMatches.replaceFirst(""); // Erase first part.
                // Match rest of it
                // Match the day for RFC Date
                var dayExpr = "(Mon|Tue|Wed|Thu|Fri|Sat|Sun)";
                var dateExpr = "([0-2]\\d|3[0-1]) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \\d{4} (([0-1]\\d|2[0-3]):[0-5]\\d:[0-5]\\d) GMT";
                var expiryExpr = "(Expires=" + dayExpr + ", " + dateExpr + ")?";
                // Match max age number with grouping [1-9] to disallow 0 as start and \d* for the rest of it
                var maxAgeExpr = "(?:Max-Age=[1-9]\\d*)?";
                var domainExpr = "(?:Domain=(\\.?[A-Za-z]([A-Za-z0-9\\-]*[A-Za-z0-9])?)*)?";
                // Any chars except ;
                var pathExpr = "(?:Path=(?:[^;])+)?";
                //Match Secure and HttpOnly
                var httpSecureExpr = "(?:Secure)?(?:HttpOnly)?";

                // Combine patterns
                var remainder = Pattern.compile("(; " + expiryExpr + "|" + maxAgeExpr + "|" + domainExpr + "|" + pathExpr + "|" + httpSecureExpr + ")+");
                var remainderMatches = remainder.matcher(cookie);
                // Full match = legal, not full match = illegal
                return remainderMatches.matches();
            } else {
                return false;
            }
        }
        return false;
    }

    /**
     * Main entry
     *
     * @param args Command line arguments
     */
    public static void main(String[] args) {
        String[] cookies = {
                // Legal cookies:
                "Set-Cookie: ns1=\"alss/0.foobar^\"",                                           // 01 name=value
                "Set-Cookie: ns1=",                                                             // 02 empty value
                "Set-Cookie: ns1=\"alss/0.foobar^\"; Expires=Tue, 18 Nov 2008 16:35:39 GMT",    // 03 Expires=time_stamp
                "Set-Cookie: ns1=; Domain=",                                                    // 04 empty domain
                "Set-Cookie: ns1=; Domain=.srv.a.com-0",                                        // 05 Domain=host_name
                "Set-Cookie: lu=Rg3v; Expires=Tue, 18 Nov 2008 16:35:39 GMT; Path=/; Domain=.example.com; HttpOnly", // 06
                // Illegal cookies:
                "Set-Cookie:",                                              // 07 empty cookie-pair
                "Set-Cookie: sd",                                           // 08 illegal cookie-pair: no "="
                "Set-Cookie: =alss/0.foobar^",                              // 09 illegal cookie-pair: empty name
                "Set-Cookie: ns@1=alss/0.foobar^",                          // 10 illegal cookie-pair: illegal name
                "Set-Cookie: ns1=alss/0.foobar^;",                          // 11 trailing ";"
                "Set-Cookie: ns1=; Expires=Tue 18 Nov 2008 16:35:39 GMT",   // 12 illegal Expires value
                "Set-Cookie: ns1=alss/0.foobar^; Max-Age=01",               // 13 illegal Max-Age: starting 0
                "Set-Cookie: ns1=alss/0.foobar^; Domain=.0com",             // 14 illegal Domain: starting 0
                "Set-Cookie: ns1=alss/0.foobar^; Domain=.com-",             // 15 illegal Domain: trailing non-letter-digit
                "Set-Cookie: ns1=alss/0.foobar^; Path=",                    // 16 illegal Path: empty
                "Set-Cookie: ns1=alss/0.foobar^; httponly",                 // 17 lower case
        };

        for (int i = 0; i < cookies.length; i++)
            System.out.printf("Cookie %2d: %s%n", i + 1, verifyCookie(cookies[i]) ? "Legal" : "Illegal");
    }

}
