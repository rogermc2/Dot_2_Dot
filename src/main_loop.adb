
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Config;
with Dot_Tables;
with State_Machine;
with Types; use Types;
with Utilities;

procedure Main_Loop (Input, Output : in Ada.Text_IO.File_Type) is
   use Types.Lines;
   S      : Types.Lines.Bounded_String;
   N      : Config.Name;
   Finish : Natural := 0;
begin
   loop
      S := Types.Lines_IO.Get_Line (Input);
      Put_Line ("Main_Loop S: " & To_String (S));
      loop
         Utilities.Get_Name (S, Finish, N);
         Put_Line ("Main_Loop Name: " & N);
         exit when Finish = 0;
         State_Machine.State_Machine (N);
      end loop;
   end loop;

exception
   when E : Syntax_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                              " expected");
      Types.Lines_IO.Put_Line (S);
   when Ada.Text_IO.End_Error =>
      Ada.Text_IO.Put_Line ("EOF");
--        Dot_Tables.Sort (Table);
--        Dot_Tables.Put (Table, Output);

end Main_Loop;

