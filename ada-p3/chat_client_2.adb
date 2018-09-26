with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Command_Line, Ada.Characters.Handling;
with Lower_Layer_UDP, Chat_Messages, Client_Handlers;

procedure Chat_Client_2 is

    package T_IO renames             Ada.Text_IO;
    package  ASU renames   Ada.Strings.Unbounded;
    package  ACL renames        Ada.Command_Line;
    package  ACH renames Ada.Characters.Handling;
    package  LLU renames         Lower_Layer_UDP;
    package   CM renames           Chat_Messages;

    Usage_Error, Nick_Error, Nick_In_Use, Server_Unreachable : Exception;

    type T_Client is record
        Receive :   LLU.End_Point_Type;
        Handler :   LLU.End_Point_Type;
        Nick    : ASU.Unbounded_String;
    end record;

    Quit_Command : String := ".quit";
    Usage_Message: String := "./chat_client_2 <server_host> <server_port> <nickname>";

    procedure Tie_Server (EP : out LLU.End_Point_Type) is
        Port     :              Natural;
        Host, IP : ASU.Unbounded_String;
    begin

        Host := ASU.To_Unbounded_String (ACL.Argument(1));
        IP   := ASU.To_Unbounded_String (LLU.To_IP(ASU.To_String (Host)));
        Port := Natural'Value (ACL.Argument(2));
        EP   := LLU.Build (ASU.To_String (IP),Port);

    end Tie_Server;

    procedure Create_Client (Client : out T_Client) is
    begin

        Client.Nick := ASU.To_Unbounded_String (ACH.To_Lower(ACL.Argument(3)));

        if ASU.To_String (Client.Nick) = "server" then
            raise Nick_Error;
        end if;

        LLU.Bind_Any (Client.Receive);
        LLU.Bind_Any (Client.Handler, Client_Handlers.Handler'Access);

    end Create_Client;

    procedure Init_Message (P_Buffer : access LLU.Buffer_Type;
                            Client   : in            T_Client) is
    begin

        CM.Message_Type'Output (P_Buffer, CM.Init);
        LLU.End_Point_Type'Output (P_Buffer, Client.Receive);
        LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
        ASU.Unbounded_String'Output (P_Buffer, Client.Nick);

    end Init_Message;

    procedure Writer_Message (P_Buffer : access  LLU.Buffer_Type;
                              Client   : in             T_Client;
                              Request  : in ASU.Unbounded_String) is
    begin

        CM.Message_Type'Output (P_Buffer, CM.Writer);
        LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
        ASU.Unbounded_String'Output (P_Buffer, Client.Nick);
        ASU.Unbounded_String'Output (P_Buffer, Request);

    end Writer_Message;

    procedure Logout_Message (P_Buffer : access LLU.Buffer_Type;
                              Client   : in            T_Client) is
    begin

        CM.Message_Type'Output (P_Buffer, CM.Logout);
        LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
        ASU.Unbounded_String'Output (P_Buffer, Client.Nick);

    end Logout_Message;

    procedure Receive_Welcome (P_Buffer : access LLU.Buffer_Type;
                               Accepted : out            Boolean) is
        Mess : CM.Message_Type;
    begin

        Mess     := CM.Message_Type'Input (P_Buffer);
        Accepted := Boolean'Input (P_Buffer);

    end Receive_Welcome;

    procedure Init_Session (P_Buffer  : access LLU.Buffer_Type;
                            Server_EP : in  LLU.End_Point_Type;
                            Client    : in            T_Client;
                            Accepted  : out            Boolean) is
        Expired : Boolean;
    begin

        LLU.Reset (P_Buffer.all);
        Init_Message (P_Buffer, Client);
        LLU.Send (Server_EP, P_Buffer);
        LLU.Reset (P_Buffer.all);

        LLU.Receive (Client.Receive, P_Buffer, 10.0, Expired);

        if Expired then
            raise Server_Unreachable;
        else
            Receive_Welcome (P_Buffer, Accepted);
        end if;

    end Init_Session;

    procedure Send_To_Server (P_Buffer  : access LLU.Buffer_Type;
                              Server_EP : in  LLU.End_Point_Type;
                              Client    : in            T_Client) is
        Request : ASU.Unbounded_String;
    begin

        loop
            LLU.Reset (P_Buffer.all);
            T_IO.Put (">> ");
            Request := ASU.To_Unbounded_String (T_IO.Get_Line);

            if ASU.to_String (Request) /= Quit_Command then
                Writer_Message (P_Buffer, Client, Request);
                LLU.Send (Server_EP, P_Buffer);
            else
                exit;
            end if;
        end loop;

    end Send_To_Server;

    procedure Send_Logout (P_Buffer  : access LLU.Buffer_Type;
                           Server_EP : in  LLU.End_Point_Type;
                           Client    : in            T_Client) is
    begin

        Logout_Message (P_Buffer, Client);
        LLU.Send (Server_EP, P_Buffer);

    end Send_Logout;

    Accepted :                       Boolean;
    Buffer   : aliased LLU.Buffer_Type(1024);
    Server_EP:            LLU.End_Point_Type;
    Client   :                      T_Client;

begin

    if ACL.Argument_Count /= 3 then
        raise Usage_Error;
    end if;

    Tie_Server (Server_EP);
    Create_Client (Client);

    Init_Session (Buffer'Access, Server_EP, Client, Accepted);

    T_IO.Put ("Mini-Chat v2.0:");
    if Accepted then
        T_IO.Put_Line (" Welcome " & ASU.To_String (Client.Nick));

        Send_To_Server (Buffer'Access, Server_EP, Client);
        Send_Logout (Buffer'Access, Server_EP, Client);

        LLU.Reset (Buffer);
        LLU.Finalize;
    else
        raise Nick_In_Use;
    end if;

exception

    when Usage_Error | Constraint_Error =>
        T_IO.Put_Line ("Usage error: " & Usage_Message);
        LLU.Finalize;

    when Nick_Error =>
        T_IO.Put_Line ("You can't use nickname " & ASU.To_String(Client.Nick));
        LLU.Finalize;

    when Nick_In_Use =>
        T_IO.Put_Line (" IGNORED new user " & ASU.To_String(Client.Nick) &
                       ", nick already in use");
        LLU.Finalize;

    when Server_Unreachable =>
        T_IO.Put_Line ("Server unreachable");
        LLU.Finalize;

end Chat_Client_2;
