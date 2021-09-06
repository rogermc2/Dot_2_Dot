
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body Dot_Tables is

    package Element_Sort is new
      Element_Vectors.Generic_Sorting (Elements."<");

    function Trim (Source : String;
                   Side : Ada.Strings.Trim_End :=  Ada.Strings.Right)
                   return String renames Ada.Strings.Fixed.Trim;

    procedure Put (T : Table; Output  : Ada.Text_IO.File_Type) is
        use Ada.Text_IO;
        procedure Put_Attribute (Position : Attribute_Maps.Cursor) is
        begin
            Put (Output, Trim (Attribute_Maps.Key (Position)) & " = " &
                   Trim (Attribute_Maps.Element (Position)) & ' ');
        end Put_Attribute;

        procedure Put_Element (Position : Element_Vectors.Cursor) is
            E : Elements.Element renames Element_Vectors.Element (Position);
        begin
            Put (Output, Trim (E.Source));
            if E.Target /= Elements.Blanks then
                Put (Output, Trim (E.Source));
            end if;

            if not Attribute_Maps.Is_Empty (E.Attributes) then
                Put (Output, " [ ");
                Attribute_Maps.Iterate (E.Attributes, Put_Attribute'Access);
                Put (Output, " ] ");
            end if;
            Put_Line (Output, ";");
        end Put_Element;

    begin
        Put_Line (Output, "digraph"  & Trim (T.Graph_Name) & " {");
        for A in Attribute loop
            Put (Output, "  " &
                   Ada.Characters.Handling.To_Lower (Attribute'Image (A) &
                     " [ "));
            Attribute_Maps.Iterate (T.Attribut_Map_Array (A), Put_Attribute'Access);
            Put_Line (Output, "]");
        end loop;

        Element_Vectors.Iterate (T.Nodes, Put_Element'Access);
        Element_Vectors.Iterate (T.Edges, Put_Element'Access);
        Put_Line (Output, "}");
    end Put;

    procedure Sort (T : in out Table) is
    begin
        Element_Sort.Sort (T.Edges);
        Element_Sort.Sort (T.Nodes);
    end Sort;

end Dot_Tables;
