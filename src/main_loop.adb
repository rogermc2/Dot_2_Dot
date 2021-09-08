
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
   Run    : Boolean := True;
begin
   while Run loop
      S := Types.Lines_IO.Get_Line (Input);
      while Run loop
         Utilities.Get_Name (S, Finish, N);
         Run := Finish /= 0;
         State_Machine.State_Machine (N);
      end loop;
   end loop;

exception
   when E : Syntax_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                              " expected");
      Types.Lines_IO.Put_Line (S);
   when Ada.Text_IO.End_Error =>
      null;
--        Dot_Tables.Sort (Table);
--        Dot_Tables.Put (Table, Output);

end Main_Loop;

