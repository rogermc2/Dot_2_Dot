
with Ada.Strings.Bounded;

with Config;
with Types; use Types;

package Utilities is

   procedure Get_Name (aLine   : in out Lines.Bounded_String;
                       Finish  : in out Natural;
                       N       : out Config.Name);
   function Pad (S : in String) return Config.Name;

end Utilities;
