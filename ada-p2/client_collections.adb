with Ada.Unchecked_Deallocation;

package body Client_Collections is

    procedure Free is new Ada.Unchecked_Deallocation(Cell,Cell_A);

    procedure Add_Client(Collection: in out Collection_Type; EP: in LLU.End_Point_Type;
                         Nick: in ASU.Unbounded_String; Unique: in Boolean) is
        Added: Boolean := False;
        Client_Aux: Cell_A;
    begin
        if Collection.P_First = null then
            Client_Aux := new Cell'(EP,Nick,null);
            Collection.P_First := Client_Aux;
            Collection.Total := 1;
        else
            Client_Aux := Collection.P_First;
            while Client_Aux /= null and not Added loop
                if ASU.To_String(Nick) = ASU.To_String(Client_Aux.Nick) and Unique then
                    Added := True;
                else
                    Client_Aux := Client_Aux.Next;
                end if;
            end loop;
            if not Added then
                Client_Aux := Collection.P_First;
                Collection.P_First := new Cell'(EP,Nick,Client_Aux);
                Collection.Total := Collection.Total + 1;
            else
                raise Client_Collection_Error;
            end if;
        end if;
    end Add_Client;

    procedure Delete_Client(Collection: in out Collection_Type;
                            Nick: in ASU.Unbounded_String) is
        Delete: Boolean := False;
        Client_Ant: Cell_A;
        Client_Aux: Cell_A;
    begin
        if Collection.P_First /= null then
            if ASU.To_String(Nick) = ASU.To_String(Collection.P_First.Nick) then
                Client_Aux := Collection.P_First;
                Collection.P_First := Collection.P_First.Next;
                Free(Client_Aux);
                Delete := True;
                Collection.Total := Collection.Total - 1;
            else
                Client_Ant := Collection.P_First;
                Client_Aux := Collection.P_First.Next;
                while Client_Aux /= null and not Delete loop
                    if ASU.To_String(Nick) = ASU.To_String(Client_Aux.Nick) then
                        Delete := True;
                        Client_Ant.Next := Client_Aux.Next;
                        Free(Client_Aux);
                        Collection.Total := Collection.Total - 1;
                    else
                        Client_Ant := Client_Aux;
                        Client_Aux := Client_Aux.Next;
                    end if;
                end loop;
            end if;
        end if;
        if not Delete then
            raise Client_Collection_Error;
        end if;
    end Delete_Client;

    function Search_Client(Collection: in Collection_Type;
                           EP: in LLU.End_Point_Type) return ASU.Unbounded_String is
        Aux: Cell_A := Collection.P_First;
        Nick: ASU.Unbounded_String := ASU.To_Unbounded_String("");
    begin
        while Aux /= null loop
            if LLU.Image(Aux.Client_EP) = LLU.Image(EP) then
                Nick := Aux.Nick;
                exit;
            else
                Aux := Aux.Next;
            end if;
        end loop;

        return Nick;
    end Search_Client;

    function Search_Client_EP(Collection: in Collection_Type;
                              Nick: in ASU.Unbounded_String) return LLU.End_Point_Type is
        EP: LLU.End_Point_Type;
        Found: Boolean := False;
        Aux: Cell_A := Collection.P_First;
    begin
        while Aux /= null loop
            if ASU.To_String(Nick) = ASU.To_String(Aux.Nick) then
                EP := Aux.Client_EP;
                Found := True;
                exit;
            else
                Aux := Aux.Next;
            end if;
        end loop;

        if Found then
            return EP;
        else
            raise Client_Collection_Error;
        end if;
    end Search_Client_EP;

    procedure Send_To_All(Collection: in Collection_Type;
                          P_Buffer: access LLU.Buffer_Type) is
        Client_Aux: Cell_A := Collection.P_First;
    begin
        while Client_Aux /= null loop
            LLU.Send(Client_Aux.Client_EP,P_Buffer);
            Client_Aux := Client_Aux.Next;
        end loop;
    end Send_To_All;

    function Collection_Image(Collection: in Collection_Type) return String is
        Client_Aux: Cell_A;
        EP: ASU.Unbounded_String;
        IP: ASU.Unbounded_String;
        Port: ASU.Unbounded_String;
        Client_Image: ASU.Unbounded_String;
        Concatenation: ASU.Unbounded_String := ASU.To_Unbounded_String("");
    begin
        Client_Aux := Collection.P_First;
        while Client_Aux /= null loop
            EP := ASU.To_Unbounded_String(LLU.Image(Client_Aux.Client_EP));
            ASU.Tail(EP,ASU.Length(EP)-ASU.Index(EP,":") - 1);
            IP := ASU.Head(EP,ASU.Index(EP,",")-1);
            ASU.Tail(EP,ASU.Length(EP)-ASU.Index(EP,":"));
            while ASU.Index(EP," ") /= 0 loop
                ASU.Tail(EP,ASU.Length(EP)-ASU.Index(EP," "));
            end loop;
            Port := EP;
            Client_Image := ASU.To_Unbounded_String(ASU.To_String(IP) & ":" &
                                                    ASU.To_String(Port) & " " &
                                                    ASU.To_String(Client_Aux.Nick));
            Concatenation := ASU.To_Unbounded_String(ASU.To_String(Concatenation) & 
                                                     ASCII.LF & 
                                                     ASU.To_String(Client_Image));
            Client_Aux := Client_Aux.Next;
        end loop;

        return ASU.To_String(Concatenation);
    end Collection_Image;

end Client_Collections;
