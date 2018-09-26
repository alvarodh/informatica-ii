with Ada.Unchecked_Deallocation;

package body Maps_G is

    procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_A);

    procedure Get (M       :            Map;
                   Key     : in    Key_Type;
                   Value   : out Value_Type;
                   Success : out    Boolean) is
        P_Aux : Cell_A := M.P_First;
    begin

        Success := False;

        while P_Aux /= null loop
            if Key = P_Aux.Key then
                Success := True;
                Value   := P_Aux.Value;
                exit;
            else
                P_Aux := P_Aux.Next;
            end if;
        end loop;

    end Get;

    procedure Put (M     : in out Map;
                   Key   :   Key_Type;
                   Value : Value_Type) is
        P_Aux :  Cell_A;
        Found : Boolean := False;
    begin

        if M.P_First = null then
            M.Length := 1;

            M.P_First := new Cell'(Key,Value,null);
        else
            P_Aux := M.P_First;
            while P_Aux /= null loop
                if P_Aux.Key = Key then
                    Found := True;
                    P_Aux.Value := Value;
                    exit;
                else
                    P_Aux := P_Aux.Next;
                end if;
            end loop;
            if not Found then
                if M.Length = Size then
                    raise Full_Map;
                else
                    M.Length := M.Length + 1;

                    P_Aux     := new Cell'(Key,Value,M.P_First);
                    M.P_First := P_Aux;
                end if;
            end if;
        end if;

    end Put;

    procedure Delete (M       : in out  Map;
                      Key     : in Key_Type;
                      Success : out Boolean) is
        P_Ant, P_Aux : Cell_A;
    begin

        Success := False;

        if M.P_First /= null then
            if M.P_First.Key = Key then
                M.Length := M.Length - 1;

                Success   := True;
                P_Aux     := M.P_First;
                M.P_First := M.P_First.Next;
                
                Free(P_Aux);
            else
                P_Ant := M.P_First;
                P_Aux := M.P_First.Next;
                while P_Aux /= null loop
                    if P_Aux.Key = Key then
                        M.Length := M.Length - 1;

                        Success    := True;
                        P_Ant.Next := P_Aux.Next;
                        
                        Free(P_Aux);
                        exit;
                    else
                        P_Ant := P_Aux;
                        P_Aux := P_Aux.Next;
                    end if;
                end loop;
            end if;
        end if;

    end Delete;

    function Map_Length (M : Map) return Natural is
    begin

        return M.Length;

    end Map_Length;

    function First (M : Map) return Cursor is
    begin

        return (M, M.P_First);

    end First;

    procedure Next (C : in out Cursor) is
    begin

        C.Element_A := C.Element_A.Next;

    end Next;

    function Has_Element (C : Cursor) return Boolean is
    begin

        return C.Element_A /= null;

    end Has_Element;

    function Element (C : Cursor) return Element_Type is
    begin

        if Has_Element (C) then
            return (C.Element_A.Key, C.Element_A.Value);
        else
            raise No_Element;
        end if;

    end Element;

end Maps_G;
