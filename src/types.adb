
package body Types is

    package body Elements is
        function "=" (Left, Right : Element) return Boolean is
        begin
            return False;
        end "=";

        function "<" (Left, Right : Element) return Boolean is
        begin
            return False;
        end "<";
    end Elements;

end Types;
