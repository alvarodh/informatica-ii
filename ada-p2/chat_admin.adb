with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Command_Line, Ada.Exceptions;
with Lower_Layer_UDP, Chat_Messages, Client_Collections;

procedure Chat_Admin is

    package T_IO renames Ada.Text_IO;
    package ASU renames Ada.Strings.Unbounded;
    package ACL renames Ada.Command_Line;
    package LLU renames Lower_Layer_UDP;
    package CM renames Chat_Messages;
    package CC renames Client_Collections;

    Usage_Error, Incorrect_Password: Exception;
    Usage_Message: String := "./chat_admin <server_host> <server_port> <admin_password>";

    procedure Main_Menu is
    begin
        T_IO.Put_Line("Options");
        T_IO.Put_Line("1 Show writers collection");
        T_IO.Put_Line("2 Ban writer");
        T_IO.Put_Line("3 Shutdown server");
        T_IO.Put_Line("4 Quit");
        T_IO.New_Line;
        T_IO.Put("Your option? ");
    end Main_Menu;

    procedure Create_Server(EP: out LLU.End_Point_Type;
                            Password: out ASU.Unbounded_String) is
        Port: Natural;
        Host, IP: ASU.Unbounded_String;
    begin
        Host := ASU.To_Unbounded_String(ACL.Argument(1));
        IP := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Host)));
        Port := Natural'Value(ACL.Argument(2));
        Password := ASU.To_Unbounded_String(ACL.Argument(3));
        EP := LLU.Build(ASU.To_String(IP),Port);
        T_IO.Put_Line("Server host: " & ASU.To_String(Host));
        T_IO.Put_Line("Admin password: " & ASU.To_String(Password));
    end Create_Server;

    procedure Send_Collection_Request(P_Buffer: access LLU.Buffer_Type;
                                      Admin_EP, Server_EP: in LLU.End_Point_Type;
                                      Password: in ASU.Unbounded_String) is
    begin
        CM.Message_Type'Output(P_Buffer,CM.Collection_Request);
        LLU.End_Point_Type'Output(P_Buffer,Admin_EP);
        ASU.Unbounded_String'Output(P_Buffer,Password);
        LLU.Send(Server_EP,P_Buffer);
    end Send_Collection_Request;

    procedure Receive_Collection_Data(P_Buffer: access LLU.Buffer_Type;
                                      EP: in LLU.End_Point_Type) is
        Expired: Boolean;
        Mess: CM.Message_Type;
        Data: ASU.Unbounded_String;
    begin
        LLU.Receive(EP,P_Buffer,5.0,Expired);
        if Expired then
            raise Incorrect_Password;
        else
            Mess := CM.Message_Type'Input(P_Buffer);
            Data := ASU.Unbounded_String'Input(P_Buffer);
            T_IO.New_Line;
            T_IO.Put_Line(ASU.To_String(Data));
        end if;
    end Receive_Collection_Data;

    procedure Send_Ban(P_Buffer: access LLU.Buffer_Type;
                       EP: in LLU.End_Point_Type;
                       Password, Nick: in ASU.Unbounded_String) is
    begin
        CM.Message_Type'Output(P_Buffer,CM.Ban);
        ASU.Unbounded_String'Output(P_Buffer,Password);
        ASU.Unbounded_String'Output(P_Buffer,Nick);
        LLU.Send(EP,P_Buffer);
    end Send_Ban;

    procedure Send_Shutdown(P_Buffer: access LLU.Buffer_Type;
                            Password: in ASU.Unbounded_String;
                            EP: in LLU.End_Point_Type) is
    begin
        CM.Message_Type'Output(P_Buffer,CM.Shutdown);
        ASU.Unbounded_String'Output(P_Buffer,Password);
        LLU.Send(EP,P_Buffer);
    end Send_Shutdown;

    procedure Elect_Option(P_Buffer: access LLU.Buffer_Type;
                           Password: in ASU.Unbounded_String;
                           Admin_EP, Server_EP: in LLU.End_Point_Type;
                           Finish: in out Boolean) is
        Number: Natural;
        Nick: ASU.Unbounded_String;
    begin
        Number := Natural'Value(T_IO.Get_Line);
        case Number is
            when 1 =>
                Send_Collection_Request(P_Buffer,Admin_EP,Server_EP,Password);
                LLU.Reset(P_Buffer.all);
                Receive_Collection_Data(P_Buffer,Admin_EP);
            when 2 =>
                T_IO.Put("Nick to ban? ");
                Nick := ASU.To_Unbounded_String(T_IO.Get_Line);
                Send_Ban(P_Buffer,Server_EP,Password,Nick);
            when 3 =>
                Send_Shutdown(P_Buffer,Password,Server_EP);
				T_IO.Put_Line("Server shutdown sent");
            when 4 =>
                Finish := True;
            when others =>
                T_IO.Put_Line("Try with number for 1 to 4");
        end case;
        T_IO.New_Line;
    end Elect_Option;

    Server_EP, Admin_EP: LLU.End_Point_Type;
    Buffer: aliased LLU.Buffer_Type(1024);
    Password: ASU.Unbounded_String;
    Finish: Boolean := False;

begin

    if ACL.Argument_Count /= 3 then
        raise Usage_Error;
    end if;

    Create_Server(Server_EP,Password);
    LLU.Bind_Any(Admin_EP);

    while not Finish loop
        Main_Menu;
        LLU.Reset(Buffer);
        begin
            Elect_Option(Buffer'Access,Password,Admin_EP,Server_EP,Finish);
        exception
            when Constraint_Error =>
                T_IO.New_Line;
                T_IO.Put_Line("Not an integer number");
                T_IO.New_Line;
        end;
    end loop;

    LLU.Finalize;
exception
    when Usage_Error | Constraint_Error =>
        T_IO.Put_Line("Usage error: " & Usage_Message);
        LLU.Finalize;
    when Incorrect_Password =>
        T_IO.New_Line;
        T_IO.Put_Line("Incorrect password");
        LLU.Finalize;
    when Ex:others =>
        T_IO.Put_Line("Unexpected exception: " &
                      Ada.Exceptions.Exception_Name(Ex) & " in: " &
                      Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;

end Chat_Admin;
