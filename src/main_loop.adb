
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;

with Config;
with Dot_Tables;
with State_Machine;
with Types; use Types;
with Utilities;

procedure Main_Loop (Input_Name : String) is
   use Types.Lines;
   Input       : File_Type;
   Output      : File_Type;
   --   Output file name is input file name with "-1" added
   Output_Name : String := Ada.Strings.Fixed.Insert
     (Input_Name, Ada.Strings.Fixed.Index (Input_Name, "."), "-1");
   S           : Types.Lines.Bounded_String;
   N           : Config.Name;
   Finish      : Natural := 0;
begin
   Open (Input, In_File, Input_Name);
   Create (Output, Out_File, Output_Name);

   while not End_Of_File (Input) loop
      S := Types.Lines_IO.Get_Line (Input);
      Put_Line ("Main_Loop S: " & To_String (S));
      loop
         Utilities.Get_Name (S, Finish, N);
         Put_Line ("Main_Loop Name: " & N);
         exit when Finish = 0;
         State_Machine.State_Machine (N);
      end loop;
   end loop;
   Close (Input);
   Close (Output);
   --        Dot_Tables.Sort (Table);
   --        Dot_Tables.Put (Table, Output);

exception
   when E : Syntax_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                              " expected");
      Types.Lines_IO.Put_Line (S);

end Main_Loop;

