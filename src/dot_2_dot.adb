
with Ada.Text_IO, Ada.Exceptions, Ada.Command_Line;
with Ada.Strings.Fixed, Ada.Strings.Bounded;

with Config;

with Dot_Tables;
with Main_Loop;
with Types; use Types;

procedure Dot_2_Dot is
begin
   --   Get file name from command line
   --   Output file name is input file name with "-1" added
   if Ada.Command_Line.Argument_Count /= 1 or else
     Ada.Strings.Fixed.Index(Ada.Command_Line.Argument(1), ".") = 0 then
      Ada.Text_IO.Put_Line("Usage: dot2dot file (with extension)");
   else
      -- Block used so that files and file names are not global
      declare
         use Ada.Text_IO;
         Input_Name : String  := Ada.Command_Line.Argument (1);
         Output_Name: String := Ada.Strings.Fixed.Insert
           (Input_Name, Ada.Strings.Fixed.Index (Input_Name, "."),"-1");
         Input      :  File_Type;
         Output     : File_Type;
      begin
         Open (Input, In_File, Input_Name);
         Create (Output, Out_File, Output_Name);
         Main_Loop (Input, Output);
         Close (Input);
         Close (Output);

      exception
         when Name_Error => Put_Line ("No such file" & Input_Name);
      end;
   end if;

exception
   when E : Syntax_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                              " expected");
--        Types.Lines_IO.Put_Line (S);
   when Ada.Text_IO.End_Error =>
      null;
--        Dot_Tables.Sort (Table);
--        Dot_Tables.Put (Table, Output);

end Dot_2_Dot;
