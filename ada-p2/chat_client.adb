with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Command_Line, Ada.Exceptions;
with Lower_Layer_UDP, Chat_Messages, Client_Collections;

procedure Chat_Client is

    package T_IO renames Ada.Text_IO;
    package ASU renames Ada.Strings.Unbounded;
    package ACL renames Ada.Command_Line;
    package LLU renames Lower_Layer_UDP;
    package CM renames Chat_Messages;
    package CC renames Client_Collections;

    Usage_Error: Exception;
    Usage_Message: String := "./chat_client <server_host> <server_port> <nick> [-r]";
    Quit_Command: String := ".quit";
    Private_Command: String := ".private";

    procedure Tie_Server(EP: out LLU.End_Point_Type) is
        Port: Natural;
        Host,IP: ASU.Unbounded_String;
    begin
        Host := ASU.To_Unbounded_String(ACL.Argument(1));
        Port := Natural'Value(ACL.Argument(2));
        IP := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Host)));
        EP := LLU.Build(ASU.To_String(IP),Port);
    end Tie_Server;

    procedure Send_Init(P_Buffer: access LLU.Buffer_Type;
                        Nick: in ASU.Unbounded_String;
                        Mess: in CM.Message_Type;
                        Client_EP, Server_EP: in LLU.End_Point_Type) is
    begin
        CM.Message_Type'Output(P_Buffer,Mess);
        LLU.End_Point_Type'Output(P_Buffer,Client_EP);
        ASU.Unbounded_String'Output(P_Buffer,Nick);
        LLU.Send(Server_EP,P_Buffer);
    end Send_Init;

    procedure Init_Client(P_Buffer: access LLU.Buffer_Type;
                          Nick: out ASU.Unbounded_String;
                          Mess: in CM.Message_Type;
                          Client_EP: out LLU.End_Point_Type;
                          Server_EP: in LLU.End_Point_Type) is
    begin
        LLU.Bind_Any(Client_EP);
        Nick := ASU.To_Unbounded_String(ACL.Argument(3));
        LLU.Reset(P_Buffer.all);
        Send_Init(P_Buffer,Nick,Mess,Client_EP,Server_EP);
    end Init_Client;

    procedure Separe_Request(Request: in ASU.Unbounded_String;
                             Priv, Usr, Message: out ASU.Unbounded_String) is
        Aux: ASU.Unbounded_String := Request;
    begin
        begin
            Priv := ASU.Head(Aux,ASU.Index(Aux," ") - 1);
            if ASU.To_String(Priv) = Private_Command then
                ASU.Tail(Aux,ASU.Length(Aux) - ASU.Index(Aux," "));
                Usr := ASU.Head(Aux,ASU.Index(Aux," ") - 1);
                ASU.Tail(Aux,ASU.Length(Aux) - ASU.Index(Aux," "));
                Message := Aux;
            else
                Usr := ASU.To_Unbounded_String("");
                Message := ASU.To_Unbounded_String("");
            end if;
        exception
            when Constraint_Error =>
                Priv := ASU.To_Unbounded_String("");
                Usr := ASU.To_Unbounded_String("");
                Message := ASU.To_Unbounded_String("");
        end;
    end Separe_Request;

    procedure Send_Writer(P_Buffer: access LLU.Buffer_Type;
                          Request: in ASU.Unbounded_String;
                          Client_EP, Server_EP: in LLU.End_Point_Type) is
        Priv, Usr, Message: ASU.Unbounded_String;
    begin
        Separe_Request(Request,Priv,Usr,Message);
        if ASU.To_String(Priv) = Private_Command then
            CM.Message_Type'Output(P_Buffer,CM.Writer_Private);
        else
            CM.Message_Type'Output(P_Buffer,CM.Writer);
        end if;
        LLU.End_Point_Type'Output(P_Buffer,Client_EP);
        if ASU.To_String(Usr) /= "" then
            ASU.Unbounded_String'Output(P_Buffer,Usr);
            ASU.Unbounded_String'Output(P_Buffer,Message);
        else
            ASU.Unbounded_String'Output(P_Buffer,Request);
        end if;
        LLU.Send(Server_EP,P_Buffer);
    end Send_Writer;

    procedure Receive_Server(P_Buffer: access LLU.Buffer_Type) is
        Mess: CM.Message_Type;
        Nick, Message: ASU.Unbounded_String;
    begin
        Mess := CM.Message_Type'Input(P_Buffer);
        Nick := ASU.Unbounded_String'Input(P_Buffer);
        Message := ASU.Unbounded_String'Input(P_Buffer);
        T_IO.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Message));
    end Receive_Server;

    procedure Writer_Mode(P_Buffer: access LLU.Buffer_Type;
                          Client_EP, Server_EP: in LLU.End_Point_Type) is
        Request: ASU.Unbounded_String;
    begin
        loop
            LLU.Reset(P_Buffer.all);
            Ada.Text_IO.Put("Message: ");
            Request := ASU.To_Unbounded_String(T_IO.Get_Line);
            if ASU.To_String(Request) /= Quit_Command then
                Send_Writer(P_Buffer,Request,Client_EP,Server_EP);
            else
                exit;
            end if;
        end loop;
    end Writer_Mode;

    procedure Reader_Mode(P_Buffer: access LLU.Buffer_Type;
                          EP: in LLU.End_Point_Type) is
    begin
        loop
            LLU.Reset(P_Buffer.all);
            LLU.Receive(EP,P_Buffer);
            Receive_Server(P_Buffer);
        end loop;
    end Reader_Mode;

    Mess: CM.Message_Type;
    Nick: ASU.Unbounded_String;
    Buffer: aliased LLU.Buffer_Type(1024);
    Client_EP, Server_EP: LLU.End_Point_Type;

begin

    Tie_Server(Server_EP);

    case ACL.Argument_Count is
        when 3 =>
            Mess := CM.InitW;
            Init_Client(Buffer'Access,Nick,Mess,Client_EP,Server_EP);
            Writer_Mode(Buffer'Access,Client_EP,Server_EP);
        when 4 =>
            if ACL.Argument(4) /= "-r" then
                raise Usage_Error;
            else
                Mess := CM.InitR;
                Init_Client(Buffer'Access,Nick,Mess,Client_EP,Server_EP);
                Reader_Mode(Buffer'Access,Client_EP);
            end if;
        when others =>
            raise Usage_Error;
    end case;

    LLU.Finalize;

exception
    when Usage_Error | Constraint_Error =>
        T_IO.Put_Line("Usage error: " & Usage_Message);
        LLU.Finalize;
    when Ex:others =>
        T_IO.Put_Line("Unexpected exception: " &
                      Ada.Exceptions.Exception_Name(Ex) & " in: " &
                      Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;

end Chat_Client;
