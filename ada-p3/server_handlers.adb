with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Command_Line, Ada.Calendar;
with Chat_Messages, Maps_G, Gnat.Calendar.Time_IO;

package body Server_Handlers is

    package T_IO renames           Ada.Text_IO;
    package  ASU renames Ada.Strings.Unbounded;
    package  ACL renames      Ada.Command_Line;
    package   CM renames         Chat_Messages;
    package   AC renames          Ada.Calendar;
    package  GCT renames Gnat.Calendar.Time_IO;

    use type CM.Message_Type;
    use type         AC.Time;

    Not_Server_Message: Exception;

    type T_Value is record
        Handler :   LLU.End_Point_Type;
        Info    : ASU.Unbounded_String;
        Time    :              AC.Time;
    end record;

    type T_Client is record
        Receive :   LLU.End_Point_Type;
        Handler :   LLU.End_Point_Type;
        Nick    : ASU.Unbounded_String;
    end record;

    Max_Clients: Natural := Natural'Value(ACL.Argument(2));

    package T_Active_Clients is new Maps_G (Key_Type   => ASU.Unbounded_String,
                                            Value_Type =>              T_Value,
                                            "="        =>              ASU."=", 
                                            Size       =>          Max_Clients);

    package T_Old_Clients    is new Maps_G (Key_Type   => ASU.Unbounded_String,
                                            Value_Type =>              AC.Time,
                                            "="        =>              ASU."=");

    package TAC renames T_Active_Clients;
    package TOC renames    T_Old_Clients;

    Active_Clients : TAC.Map;
    Old_Clients    : TOC.Map;

    Init_Msg : String  := " joins to the chat";
    Log_Msg  : String  := " leaves the chat";
    Ban_Msg  : String  := " banned for being idle too long";

    Server_Nick : ASU.Unbounded_String := ASU.To_Unbounded_String ("server");

    procedure Receive_Init (P_Buffer : access LLU.Buffer_Type;
                            Client   : out           T_Client) is
    begin

        Client.Receive := LLU.End_Point_Type'Input (P_Buffer);
        Client.Handler := LLU.End_Point_Type'Input (P_Buffer);
        Client.Nick    := ASU.Unbounded_String'Input (P_Buffer);

    end Receive_Init;

    procedure Send_Welcome (P_Buffer : access LLU.Buffer_Type;
                            Client   : in            T_Client;
                            Accepted : in             Boolean) is
    begin

        CM.Message_Type'Output (P_Buffer, CM.Welcome);
        Boolean'Output (P_Buffer, Accepted);

        LLU.Send(Client.Receive, P_Buffer);

    end Send_Welcome;

    procedure Receive_Writer (P_Buffer      : access   LLU.Buffer_Type;
                              EP            : out   LLU.End_Point_Type;
                              Nick, Request : out ASU.Unbounded_String) is
    begin

        EP      := LLU.End_Point_Type'Input (P_Buffer);
        Nick    := ASU.Unbounded_String'Input (P_Buffer);
        Request := ASU.Unbounded_String'Input (P_Buffer);

    end Receive_Writer;

    procedure Receive_Logout (P_Buffer : access LLU.Buffer_Type;
                              EP       : out LLU.End_Point_Type;
                              Nick     : out ASU.Unbounded_String) is
    begin

        EP   := LLU.End_Point_Type'Input (P_Buffer);
        Nick := ASU.Unbounded_String'Input (P_Buffer);

    end Receive_Logout;

    function Get_IP (EP : LLU.End_Point_Type) return String is
        IP, EP_Image : ASU.Unbounded_String;
    begin

        EP_Image := ASU.To_Unbounded_String (LLU.Image (EP));
        IP       := ASU.Head (EP_Image, ASU.Index (EP_Image, ",") - 1);

        ASU.Tail (IP, ASU.Length (IP) - ASU.Index (IP, ":") - 1);

        return ASU.To_String(IP);

    end Get_IP;

    function Get_Port (EP : LLU.End_Point_Type) return String is
        Port, EP_Image : ASU.Unbounded_String;
    begin

        EP_Image := ASU.To_Unbounded_String (LLU.Image (EP));
        Port     := ASU.Tail (EP_Image, ASU.Length (EP_Image) - ASU.Index (EP_Image,","));

        ASU.Tail (Port, ASU.Length (Port) - ASU.Index (Port, ":") - 2);

        return ASU.To_String(Port);

    end Get_Port;

    function Time_Image (T : AC.Time) return String is
    begin

        return GCT.Image(T, "%d-%b-%y %T.%i");

    end Time_Image;

    function Active_Client_Info (Client : in T_Client) return ASU.Unbounded_String is
    begin

        return ASU.To_Unbounded_String ("(" & Get_IP (Client.Handler) & ":" &
                                        Get_Port (Client.Handler) & ")");

    end Active_Client_Info;

    procedure Send_Server (P_Buffer : access LLU.Buffer_Type;
                           EP       : in  LLU.End_Point_Type;
                           Clients  : in             TAC.Map) is
        C :       TAC.Cursor := TAC.First (Clients);
        E : TAC.Element_Type;
    begin

        begin

            loop
                E := TAC.Element (C);

                if LLU.Image (E.Value.Handler) /= LLU.Image (EP) then
                    LLU.Send (E.Value.Handler, P_Buffer);
                end if;

                TAC.Next (C);
            end loop;

        exception
            when TAC.No_Element =>
                null;
        end;

    end Send_Server;

    procedure Print_Active_Clients is
        C :       TAC.Cursor := TAC.First (Active_Clients);
        E : TAC.Element_Type;
    begin

        begin

            loop
                E := TAC.Element (C);

                T_IO.Put_Line (ASU.To_String (E.Key) & ASU.To_String (E.Value.Info) &
                               ": " & Time_Image(E.Value.Time));

                TAC.Next (C);
            end loop;

        exception
            when TAC.No_Element =>
                null;
        end;

    end Print_Active_Clients;

    procedure Print_Old_Clients is
        C :       TOC.Cursor := TOC.First (Old_Clients);
        E : TOC.Element_Type;
    begin

        begin

            loop
                E := TOC.Element (C);

                T_IO.Put_Line (ASU.To_String (E.Key) & ": " & Time_Image (E.Value));

                TOC.Next (C);
            end loop;

        exception
            when TOC.No_Element =>
                null;
        end;

    end Print_Old_Clients;

    procedure Prepare_Server_Message (P_Buffer      : access  LLU.Buffer_Type;
                                      Request, Nick : in ASU.Unbounded_String) is
    begin

        CM.Message_Type'Output (P_Buffer, CM.Server);
        ASU.Unbounded_String'Output (P_Buffer, Nick);
        ASU.Unbounded_String'Output (P_Buffer, Request);

    end Prepare_Server_Message;

    procedure Oldest_Client (Clients     : in               TAC.Map;
                             EP          : out   LLU.End_Point_Type;
                             Nick, Value : out ASU.Unbounded_String) is
        C :       TAC.Cursor := TAC.First (Clients);
        E : TAC.Element_Type;
        T :          AC.Time;
    begin

        E     := TAC.Element (C);
        EP    := E.Value.Handler;
        Value := E.Value.Info;
        Nick  := E.Key;
        T     := E.Value.Time;

        loop
            E := TAC.Element (C);

            if T > E.Value.Time then
                EP    := E.Value.Handler;
                Value := E.Value.Info;
                Nick  := E.Key;
                T     := E.Value.Time;
            end if;

            TAC.Next (C);
        end loop;

    exception
        when TAC.No_Element =>
            null;

    end Oldest_Client;

    procedure Ban_Client (P_Buffer       : access LLU.Buffer_Type;
                          Active_Clients : in out         TAC.Map;
                          Old_Clients    : in out         TOC.Map) is
        Old_EP               :   LLU.End_Point_Type;
        Accepted             :              Boolean;
        Value, Nick, Request : ASU.Unbounded_String;
        T                    :              AC.Time;
    begin

        Oldest_Client (Active_Clients, Old_EP, Nick, Value);

        T := AC.Clock;

        TAC.Delete (Active_Clients, Nick, Accepted);

        TOC.Put (Old_Clients, Nick, T);

        Request := ASU.To_Unbounded_String (ASU.To_String (Nick) & Ban_Msg);

        LLU.Reset (P_Buffer.all);

        Prepare_Server_Message (P_Buffer, Request, Server_Nick);
        Send_Server (P_Buffer, Old_EP, Active_Clients);

        LLU.Send (Old_EP, P_Buffer);

    end Ban_Client;

    procedure Save_Client (P_Buffer       : access LLU.Buffer_Type;
                           Active_Clients : in out         TAC.Map;
                           Old_Clients    : in out         TOC.Map;
                           Client         : in            T_Client) is
        Success :              Boolean;
        Request : ASU.Unbounded_String;
        Value   :              T_Value;
    begin

        TAC.Get (Active_Clients, Client.Nick, Value, Success);

        if not Success then
            Value.Info    := Active_Client_Info (Client);
            Value.Handler := Client.Handler;
            Value.Time    := AC.Clock;
            Request       := ASU.To_Unbounded_String (ASU.To_String (Client.Nick) &
                                                      Init_Msg);

            TAC.Put (Active_Clients, Client.Nick, Value);

            LLU.Reset (P_Buffer.all);

            Prepare_Server_Message (P_Buffer, Request, Server_Nick);
            Send_Server (P_Buffer, Client.Handler, Active_Clients);

            T_IO.Put_Line ("ACCEPTED");
        else
            T_IO.Put_Line ("IGNORED. nick already used");
        end if;

        LLU.Reset (P_Buffer.all);

        Send_Welcome (P_Buffer, Client, not Success);

    end Save_Client;

    procedure Full_List (P_Buffer       : access LLU.Buffer_Type;
                         Active_Clients : in out         TAC.Map;
                         Old_Clients    : in out         TOC.Map;
                         Client         : in            T_Client) is
        Request : ASU.Unbounded_String;
        Value   :              T_Value;
    begin

        Value.Info    := Active_Client_Info (Client);
        Value.Handler := Client.Handler;
        Value.Time    := AC.Clock;
        Request       := ASU.To_Unbounded_String (ASU.To_String (Client.Nick) & Init_Msg);

        Ban_Client (P_Buffer, Active_Clients, Old_Clients);
        TAC.Put (Active_Clients, Client.Nick, Value);

        LLU.Reset (P_Buffer.all);

        Prepare_Server_Message (P_Buffer, Request, Server_Nick);
        Send_Server (P_Buffer, Client.Handler, Active_Clients);

        LLU.Reset (P_Buffer.all);

        Send_Welcome (P_Buffer, Client, True);

        T_IO.Put_Line ("ACCEPTED");

    end Full_List;

    procedure Init_Message (P_Buffer       : access LLU.Buffer_Type;
                            Active_Clients : in out         TAC.Map;
                            Old_Clients    : in out         TOC.Map) is
        Client : T_Client;
    begin

        Receive_Init (P_Buffer, Client);
        T_IO.Put (ASU.To_String(Client.Nick) & ": ");

        begin

            Save_Client (P_Buffer, Active_Clients, Old_Clients, Client);

        exception

            when TAC.Full_Map =>
                Full_List (P_Buffer, Active_Clients, Old_Clients, Client);
        end;

    end Init_Message;

    procedure Writer_Message (P_Buffer       : access LLU.Buffer_Type;
                              Active_Clients : in out         TAC.Map;
                              Old_Clients    : in out         TOC.Map) is
        Client   :             T_Client;
        Accepted :              Boolean;
        Request  : ASU.Unbounded_String;
        Value    :              T_Value;
    begin

        Receive_Writer (P_Buffer, Client.Handler, Client.Nick, Request);
        TAC.Get (Active_Clients, Client.Nick, Value, Accepted);

        if Accepted then
            T_IO.Put_Line (ASU.To_String (Client.Nick) & ": " & ASU.To_String (Request));

            Value.Info := Active_Client_Info (Client);
            Value.Time := AC.Clock;

            TAC.Put (Active_Clients, Client.Nick, Value);

            LLU.Reset (P_Buffer.all);

            Prepare_Server_Message (P_Buffer, Request, Client.Nick);
            Send_Server (P_Buffer, Client.Handler, Active_Clients);
        else
            T_IO.Put_Line ("unknown client. IGNORED");
        end if;

    end Writer_Message;

    procedure Logout_Message (P_Buffer       : access LLU.Buffer_Type;
                              Active_Clients : in out         TAC.Map;
                              Old_Clients    : in out         TOC.Map) is
        Client   :             T_Client;
        Accepted :              Boolean;
        Request  : ASU.Unbounded_String;
        A_Value  :              T_Value;
        T        :              AC.Time;
    begin

        Receive_Logout (P_Buffer, Client.Handler, Client.Nick);
        TAC.Get (Active_Clients, Client.Nick, A_Value, Accepted);

        if Accepted and then LLU.Image(A_Value.Handler) = LLU.Image(Client.Handler) then
            T_IO.Put_Line (ASU.To_String (Client.Nick));

            TAC.Delete (Active_Clients, Client.Nick, Accepted);

            T := AC.Clock;

            TOC.Put (Old_Clients, Client.Nick, T);

            LLU.Reset (P_Buffer.all);

            Request := ASU.To_Unbounded_String (ASU.To_String (Client.Nick) & Log_Msg);

            Prepare_Server_Message (P_Buffer, Request, Server_Nick);
            Send_Server (P_Buffer, Client.Handler, Active_Clients);
        else
            T_IO.Put_Line("unknown client. IGNORED");
        end if;

    end Logout_Message;

    procedure Handler (From     : in  LLU.End_Point_Type;
                       To       : in  LLU.End_Point_Type;
                       P_Buffer : access LLU.Buffer_Type) is
        Mess : CM.Message_Type;
    begin

        Mess := CM.Message_Type'Input (P_Buffer);
        T_IO.Put (CM.Message_Type'Image (Mess) & " received from ");

        case Mess is
            when CM.Init =>
                Init_Message (P_Buffer, Active_Clients, Old_Clients);

            when CM.Writer =>
                Writer_Message (P_Buffer, Active_Clients, Old_Clients);

            when CM.Logout =>
                Logout_Message (P_Buffer, Active_Clients, Old_Clients);

            when others =>
                null;

        end case;

        LLU.Reset (P_Buffer.all);

    end Handler;

end Server_Handlers;
