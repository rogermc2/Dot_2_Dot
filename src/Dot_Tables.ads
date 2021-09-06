
with Ada.Text_IO;

with Types; use Types;
with Config;

package Dot_Tables is
        type Attribute is (Graphs, Nodes, Edges);
        type Attribut_Map_Arrays is array (Attribute) of Attribute_Maps.Map;

        type Table is record
            Graph_Name         : Config.Name;
            Attribut_Map_Array : Attribut_Map_Arrays;
            Nodes              : Element_Vectors.Vector;
            Edges              : Element_Vectors.Vector;
        end record;

        procedure Put (T : Table; Output  : Ada.Text_IO.File_Type);
        procedure Sort (T : in out Table);

end Dot_Tables;
