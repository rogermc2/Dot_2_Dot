
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;

with Config;
with Dot_Tables;
with State_Machine;
with Types; use Types;
with Utilities;

procedure Main_Loop (Input_File_Name : String) is
   use Types.Lines;
   Table    : Dot_Tables.Table_Data;
   Input    : File_Type;
   Output   : File_Type;
   --  The output file name is the input file name with "-1" added.
   Output_File_Name : String := Ada.Strings.Fixed.Insert
     (Input_File_Name, Ada.Strings.Fixed.Index (Input_File_Name, "."), "-1");
   S        : Types.Lines.Bounded_String;
   N        : Config.Name;
   Finish   : Natural := 0;
   Continue : Boolean := True;
begin
   Open (Input, In_File, Input_File_Name);
   Create (Output, Out_File, Output_File_Name);

   while not End_Of_File (Input) loop
      S := Types.Lines_IO.Get_Line (Input);
      Continue := True;
      while Continue loop
         Utilities.Get_Name (S, Finish, N);
         Continue := Finish /= 0;
         if Continue then
            State_Machine.Parse_Line (Table, N);
         end if;
      end loop;
   end loop;

   Close (Input);

   Dot_Tables.Sort (Table);
   Dot_Tables.Put (Table, Output);
   Close (Output);

exception
   when E : Syntax_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                              " expected");
      Types.Lines_IO.Put_Line (S);

end Main_Loop;

