
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Config;

with Dot_Tables;
with Main_Loop;
with Types; use Types;

procedure Dot_2_Dot is

    File_Name : Ada.Strings.Unbounded.Unbounded_String;

    procedure Run is
        use Ada.Text_IO;
        Input_Name : String := To_String (File_Name);
        --   Output file name is input file name with "-1" added
        Output_Name : String := Ada.Strings.Fixed.Insert
          (Input_Name, Ada.Strings.Fixed.Index (Input_Name, "."), "-1");
        Input  :  File_Type;
        Output : File_Type;
    begin
        Open (Input, In_File, Input_Name);
        Create (Output, Out_File, Output_Name);
        Main_Loop (Input, Output);
        Close (Input);
        Close (Output);

    exception
        when Name_Error => Put_Line ("No such file" & Input_Name);
    end Run;

begin
    --   Get file name from command line
    Ada.Text_IO.Put ("Dot_2_Dot: Enter File Name: ");
    File_Name := To_Unbounded_String (Ada.Text_IO.Get_Line);
    Run;

exception
    when E : Syntax_Error =>
        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                                " expected");

end Dot_2_Dot;
