-- State machine for recognizing and storing Dot input
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Dot_Tables;
with Utilities;

package body State_Machine is

    Current_State     : State := Digraph;
    Current_Statement : Statement;
    Current_Attribute : Dot_Tables.Attribute;
    Current_Name      : Config.Name;
    Table             : Dot_Tables.Table_Data;

    procedure State_Machine (N : Config.Name) is
        use Utilities;
        E : Types.Elements.Element;
    begin
        Put_Line ("State_Machine Current_State: " &
                    State'Image (Current_State));
        case Current_State is
        -- First line "digraph name {"
        when Digraph =>
            if N = Utilities.Pad ("digraph") then
                Current_State := Name;
            else
                raise Syntax_Error with "digraph";
            end if;

        when Name =>
            Table.Graph_Name := N;
            Current_State := Open_Brace;

        when Open_Brace =>
            if N = Pad ("{") then
                Current_State := Statements;
                Put_Line ("State_Machine Open_Brace new Current_State: " &
                            State'Image (Current_State));
            else
                raise Syntax_Error with "{";
            end if;

            -- Statement
            --   graph | node | edge [ list of key = value ];
        when Statements =>
            -- Check for end of input
            if N = Pad ("}") then
                null;
                -- Three possible attributes
            elsif N = Pad ("graph") then
                Current_Statement := Attributes;
                Current_Attribute := Dot_Tables.Graphs;
                Current_State := Open_Bracket;

            elsif N = Pad ("node") then
                Current_Statement := Attributes;
                Current_Attribute := Dot_Tables.Nodes;
                Current_State := Open_Bracket;

            elsif N = Pad ("edge") then
                Current_Statement := Attributes;
                Current_Attribute := Dot_Tables.Edges;
                Current_State := Open_Bracket;
                -- Node or edge: store name and look for edge pointer

            else
                Current_Name := N;
                Current_State := Pointer;
            end if;

        when Pointer =>
            -- If edge pointer, look for target
            if N = Pad ("->") then
                Current_Statement := Edges;
                Current_State := Targets;
                -- Must be a node, store in element vector
            else
                Element_Vectors.Append (
                                        Table.Nodes,
                                        (Source => Current_Name,
                                         Target => <>,
                                         Attributes => <>));
                Current_Statement := Nodes;
                -- Attribute list after node
                if N = Pad ("[") then
                    Current_State := Attributes;
                elsif N = Pad (";") then
                    Current_State := Statements;
                else raise Syntax_Error with "[ or ;";
                end if;
            end if;

            -- Target of an edge, store in element vector
        when Targets =>
            Element_Vectors.Append (
                                    Table.Edges,
                                    (Source => Current_Name,
                                     Target => N,
                                     Attributes => <>));
            Current_State := Open_Bracket;

            -- Get attribute list
            --   List opens with bracket
        when Open_Bracket =>
            if N = Pad ("[") then
                Current_State := Attributes;
            elsif N = Pad (";") then
                Current_State := Statements;
            else
                raise Syntax_Error with "[ or ;"; end if;
            -- List closes with bracket; otherwise, save key
        when Attributes =>
            if N = Pad ("]") then
                Current_State := Semicolon;
            else
                Current_Name := N;
                Current_State := Equals;
            end if;
        when Equals =>
            if N = Pad ("=") then
                Current_State := Values;
            else
                raise Syntax_Error with "=";
            end if;
            -- Value found, insert key and value in attribute list
        when Values =>
            -- graph | node | edge attribute list
            if Current_Statement = Attributes then
                Attribute_Maps.Insert
                  (Table.Attribute_Map_Array (Current_Attribute), Current_Name, N);
                -- Attribute list associate with a node
                -- Put list in last element found
            elsif Current_Statement = Nodes then
                E := Element_Vectors.Last_Element (Table.Nodes);
                Attribute_Maps.Insert (
                                       E.Attributes,
                                       Current_Name, N);
                Element_Vectors.Replace_Element (
                                                 Table.Nodes, Element_Vectors.Last (Table.Nodes), E);
                -- Attribute list associate with an edge
                -- Put list in last element found
            elsif Current_Statement = Edges then
                E := Element_Vectors.Last_Element (Table.Edges);
                Attribute_Maps.Insert (
                                       E.Attributes,
                                       Current_Name, N);
                Element_Vectors.Replace_Element (
                                                 Table.Edges, Element_Vectors.Last (Table.Edges), E);
            end if;
            Current_State := Attributes;

            -- End of statement
        when Semicolon =>
            if N = Pad (";") then
                Current_State := Statements;
            else
                raise Syntax_Error with ";";
            end if;
        end case;

    exception
        when E : Syntax_Error =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                                    " expected");
            --           Types.Lines_IO.Put_Line (S);
        when Ada.Text_IO.End_Error =>
            Dot_Tables.Sort (Table);
            --           Dot_Tables.Put (Table, Output);
    end State_Machine;

end State_Machine;
