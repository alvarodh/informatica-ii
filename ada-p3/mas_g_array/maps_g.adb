package body Maps_G is

    procedure Get (M       :            Map;
                   Key     : in    Key_Type;
                   Value   : out Value_Type;
                   Success : out    Boolean) is
    begin

        Success := False;

        if M.Length /= 0 then
            for I in 1..Size loop
                if M.P_Array(I).Full and then M.P_Array(I).Key = Key then
                    Success := True;
                    Value   := M.P_Array(I).Value;
                    exit;
                end if;
            end loop;
        end if;

    end Get;

    procedure Put (M     : in out Map;
                   Key   :   Key_Type;
                   Value : Value_Type) is
        Found : Boolean := False;
    begin

        if M.Length = 0 then
            M.P_Array(1).Key   := Key;
            M.P_Array(1).Value := Value;
            M.P_Array(1).Full  := True;

            M.Length := 1;
        else
            for I in 1..Size loop
                if M.P_Array(I).Full and then M.P_Array(I).Key = Key then
                    M.P_Array(I).Value := Value;
                    Found              := True;
                    exit;
                end if;
            end loop;
            if not Found then
                if M.Length = Size then
                    raise Full_Map;
                else
                    M.Length := M.Length + 1;

                    M.P_Array(M.Length).Key   := Key;
                    M.P_Array(M.Length).Value := Value;
                    M.P_Array(M.Length).Full  := True;
                end if;
            end if;
        end if;

    end Put;

    procedure Delete (M       : in out  Map;
                      Key     : in Key_Type;
                      Success : out Boolean) is
    begin

        Success := False;

        for I in 1..Size loop
            if M.P_Array(I).Key = Key then
                M.P_Array(I).Full := False;
                M.P_Array(I..Size-1) := M.P_Array(I+1..Size);
                if M.P_Array(Size).Full then
                    M.P_Array(Size).Full := False;
                end if;
                Success := True;
                exit;
            end if;
        end loop;

    end Delete;

    function Map_Length(M : Map) return Natural is
    begin

        return M.Length;

    end Map_Length;

    function First(M : Map) return Cursor is
    begin

        return (M, 1);

    end First;

    procedure Next(C : in out Cursor) is
    begin

        C.Position := C.Position + 1;

    end Next;

    function Has_Element(C : Cursor) return Boolean is
    begin

        if C.Position > Size then
            return False;
        else
            return C.M.P_Array(C.Position).Full;
        end if;

    end Has_Element;

    function Element(C : Cursor) return Element_Type is
    begin

        if Has_Element(C) then
            return (C.M.P_Array(C.Position).Key, C.M.P_Array(C.Position).Value);
        else
            raise No_Element;
        end if;

    end Element;

end Maps_G;
