
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Bounded_IO;
with Ada.Containers.Ordered_Maps;

package Types is

   package Lines is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Config.Line_Length);

   package Lines_IO is new Ada.Text_IO.Bounded_IO (Lines);

   package Attribute_Maps is new
     Ada.Containers.Ordered_Maps (Config.Name, Config.Name);

end Types;
