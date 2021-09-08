
with Ada.Text_IO, Ada.Exceptions, Ada.Command_Line;
with Ada.Strings.Fixed, Ada.Strings.Bounded;

with Config;

with Dot_Tables;
with Types;

procedure Dot_2_Dot is

    -- States for recognizer
    type State is
      (Digraph, Name, Open_Brace, Statements, Open_Bracket, Equals, Semicolon,
       Attributes, Values, Pointer, Targets);
    Current_State : State := Digraph;

    -- A statement is a list of attributes or an element (node or edge)
    type Statement is (Attributes, Nodes, Edges);
    Current_Statement : Statement;
    Current_Attribute : Dot_Tables.Attribute;

    -- For edges a -> b, store first node name
    -- For attributes key = vale, store key name
    Current_Name : Config.Name;

    Syntax_Error : exception;

    Table : Dot_Tables.Table;

    -- Pad the string S so that it fits into Config.Name
    function Pad (S : in String) return Config.Name is
    begin
        return Ada.Strings.Fixed.Head (S, Config.Name'Length);
    end Pad;

    procedure State_Machine (N : in Config.Name) is separate;
    procedure Get_Name (L : in out Types.Lines.Bounded_String;
                        Finish : in out Natural;
                        N : out Config.Name) is separate;

    procedure Main_Loop (Input, Output : in Ada.Text_IO.File_Type) is
        S      : Types.Lines.Bounded_String;
        N      : Config.Name;
        Finish : Natural := 0;
    begin
        loop
            S := Types.Lines_IO.Get_Line (Input);
            loop
                Get_Name (S, Finish, N);
                exit when Finish = 0;
                State_Machine (N);
            end loop;
        end loop;

    exception
        when E : Syntax_Error =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                                    " expected");
            Types.Lines_IO.Put_Line (S);
        when Ada.Text_IO.End_Error =>
            Dot_Tables.Sort (Table);
            Dot_Tables.Put (Table, Output);
    end Main_Loop;

begin
    null;

end Dot_2_Dot;
