
with Ada.Exceptions;

with Config;
with Dot_Tables;
with State_Machine;
with Types; use Types;
with Utilities;

procedure Main_Loop (Input, Output : in Ada.Text_IO.File_Type) is
   S      : Types.Lines.Bounded_String;
   N      : Config.Name;
   Finish : Natural := 0;
begin
   loop
      S := Types.Lines_IO.Get_Line (Input);
      loop
         Utilities.Get_Name (S, Finish, N);
         exit when Finish = 0;
         State_Machine.State_Machine (N);
      end loop;
   end loop;

end Main_Loop;

