
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Main_Loop;

procedure Dot_2_Dot is
   File_Name : Ada.Strings.Unbounded.Unbounded_String;
begin
   Ada.Text_IO.Put ("Dot_2_Dot: Enter File Name: ");
   File_Name := To_Unbounded_String (Ada.Text_IO.Get_Line);
   Main_Loop (To_String (File_Name));

end Dot_2_Dot;
