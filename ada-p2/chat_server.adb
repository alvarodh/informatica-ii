with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Command_Line, Ada.Exceptions;
with Lower_Layer_UDP, Chat_Messages, Client_Collections;
use type Chat_Messages.Message_Type;

procedure Chat_Server is

    package T_IO renames Ada.Text_IO;
    package ASU renames Ada.Strings.Unbounded;
    package ACL renames Ada.Command_Line;
    package LLU renames Lower_Layer_UDP;
    package CM renames Chat_Messages;
    package CC renames Client_Collections;

    Usage_Error, Shutdown: Exception;
    Usage_Message: String := "./chat_server <server_port> <admin_password>";

    type List_Name is (Writer, Reader);
    type Client_Type is array (List_Name) of CC.Collection_Type;

    procedure Create_Server(EP: out LLU.End_Point_Type;
                            Pswrd: out ASU.Unbounded_String) is
        Port: Natural;
        Host, IP, Port_Image: ASU.Unbounded_String;
    begin

        Host := ASU.To_Unbounded_String(LLU.Get_Host_Name);
        IP := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Host)));
        Port := Natural'Value(ACL.Argument(1));
        Port_Image := ASU.To_Unbounded_String(ACL.Argument(1));
        ASU.Tail(Port_Image, ASU.Length(Port_Image) - ASU.Index(Port_Image, " "));
        Pswrd := ASU.To_Unbounded_String(ACL.Argument(2));

        EP := LLU.Build(ASU.To_String(IP),Port);
        LLU.Bind(EP);

        T_IO.Put_Line("Host name: " & ASU.To_String(Host));
        T_IO.Put_Line("Address: " & ASU.To_String(IP) & ":" & ASU.To_String(Port_Image));
        T_IO.Put_Line("Password: " & ASU.To_String(Pswrd));
        T_IO.New_Line(2);

    end Create_Server;

    procedure Receive_Init(P_Buffer: access LLU.Buffer_Type;
                           EP: out LLU.End_Point_Type;
                           Nick: out ASU.Unbounded_String) is
    begin
        EP := LLU.End_Point_Type'Input(P_Buffer);
        Nick := ASU.Unbounded_String'Input(P_Buffer);
    end Receive_Init;

    procedure Receive_Writer(P_Buffer: access LLU.Buffer_Type;
                             EP: out LLU.End_Point_Type;
                             Message: out ASU.Unbounded_String) is
    begin
        EP := LLU.End_Point_Type'Input(P_Buffer);
        Message := ASU.Unbounded_String'Input(P_Buffer);
    end Receive_Writer;

    procedure Receive_Writer_Private(P_Buffer: access LLU.Buffer_Type;
                                     EP: out LLU.End_Point_Type;
                                     Nick, Message: out ASU.Unbounded_String) is
    begin
        EP := LLU.End_Point_Type'Input(P_Buffer);
        Nick := ASU.Unbounded_String'Input(P_Buffer);
        Message := ASU.Unbounded_String'Input(P_Buffer);
    end Receive_Writer_Private;

    procedure Receive_Collection_Request(P_Buffer: access LLU.Buffer_Type;
                                         EP: out LLU.End_Point_Type;
                                         Password: out ASU.Unbounded_String) is
    begin
        EP := LLU.End_Point_Type'Input(P_Buffer);
        Password := ASU.Unbounded_String'Input(P_Buffer);
    end Receive_Collection_Request;

    procedure Receive_Ban(P_Buffer: access LLU.Buffer_Type;
                          Nick, Password: out ASU.Unbounded_String) is
    begin
        Password := ASU.Unbounded_String'Input(P_Buffer);
        Nick := ASU.Unbounded_String'Input(P_Buffer);
    end Receive_Ban;

    procedure Send_Server(P_Buffer: access LLU.Buffer_Type;
                          List: in CC.Collection_Type;
                          Nick, Message: in ASU.Unbounded_String) is
    begin
        CM.Message_Type'Output(P_Buffer,CM.Server);
        ASU.Unbounded_String'Output(P_Buffer,Nick);
        ASU.Unbounded_String'Output(P_Buffer,Message);
        CC.Send_To_All(List,P_Buffer);
    end Send_Server;

    procedure Send_Private_Server(P_Buffer: access LLU.Buffer_Type;
                                  EP: in LLU.End_Point_Type;
                                  Nick: in ASU.Unbounded_String;
                                  Message: in ASU.Unbounded_String) is
    begin
        CM.Message_Type'Output(P_Buffer,CM.Server);
        ASU.Unbounded_String'Output(P_Buffer,Nick);
        ASU.Unbounded_String'Output(P_Buffer,Message);
        LLU.Send(EP,P_Buffer);
    end Send_Private_Server;

    procedure Send_Collection_Data(P_Buffer: access LLU.Buffer_Type;
                                   Message: in ASU.Unbounded_String;
                                   EP: in LLU.End_Point_Type) is
    begin
        CM.Message_Type'Output(P_Buffer,CM.Collection_Data);
        ASU.Unbounded_String'Output(P_Buffer,Message);
        LLU.Send(EP,P_Buffer);
        T_IO.New_Line;
    end Send_Collection_Data;

    procedure Save_Client(P_Buffer: access LLU.Buffer_Type;
                          EP: in LLU.End_Point_Type;
                          Lists: in out Client_Type;
                          Nick: in ASU.Unbounded_String;
                          Mess: in CM.Message_Type) is
        Unique: Boolean := True;
        Message, Server_Nick: ASU.Unbounded_String;
    begin
        if Mess = CM.InitR then
            CC.Add_Client(Lists(Reader),EP,Nick,Unique);
        else
            CC.Add_Client(Lists(Writer),EP,Nick,Unique);
            LLU.Reset(P_Buffer.all);
            Message := ASU.To_Unbounded_String(ASU.To_String(Nick) & 
                                                " joins to the chat");
            Server_Nick := ASU.To_Unbounded_String("server");
            Send_Server(P_Buffer,Lists(Reader),Server_Nick,Message);
        end if;
    end Save_Client;

    procedure Check_Client(P_Buffer: access LLU.Buffer_Type;
                           EP: in LLU.End_Point_Type;
                           Lists: in Client_Type;
                           Message: in ASU.Unbounded_String) is
        Nick: ASU.Unbounded_String;
    begin
        Nick := CC.Search_Client(Lists(Writer),EP);
        if ASU.To_String(Nick) /= "" then
            T_IO.Put_Line(" from " & ASU.To_String(Nick) & ": " & ASU.To_String(Message));
            Send_Server(P_Buffer,Lists(Reader),Nick,Message);
        else
            T_IO.Put_Line(" from unknown client. IGNORED");
        end if;
    end Check_Client;

    procedure Ban_Message(P_Buffer: access LLU.Buffer_Type;
                          List: in out CC.Collection_Type;
                          Password: in ASU.Unbounded_String) is
        Nick, Admin_Password: ASU.Unbounded_String;
    begin
        Receive_Ban(P_Buffer,Nick,Admin_Password);
        T_IO.Put(" for " & ASU.To_String(Nick));
        LLU.Reset(P_Buffer.all);
        if ASU.To_String(Admin_Password) = ASU.To_String(Password) then
            begin
                CC.Delete_Client(List,Nick);
                T_IO.New_Line;
            exception
                when CC.Client_Collection_Error =>
                    T_IO.Put_Line(". IGNORED, nick not found");
            end;
        else
            T_IO.Put_Line(". IGNORED, incorrect password");
        end if;
    end Ban_Message;

    procedure Collection_Message(P_Buffer: access LLU.Buffer_Type;
                                 List: in out CC.Collection_Type;
                                 Password: in ASU.Unbounded_String) is
        EP: LLU.End_Point_Type;
        Nick, Message, Admin_Password: ASU.Unbounded_String;
    begin
        Receive_Collection_Request(P_Buffer,EP,Admin_Password);
        LLU.Reset(P_Buffer.all);
        if ASU.To_String(Password) = ASU.To_String(Admin_Password) then
            Message := ASU.To_Unbounded_String(CC.Collection_Image(List));
            Send_Collection_Data(P_Buffer,Message,EP);
        else
            T_IO.Put_Line(". IGNORED, incorrect password");
        end if;
    end Collection_Message;

    procedure Shutdown_Message(P_Buffer: access LLU.Buffer_Type;
                               Password: in ASU.Unbounded_String) is
        Pswrd: ASU.Unbounded_String;
    begin
        Pswrd := ASU.Unbounded_String'Input(P_Buffer);
        if ASU.To_String(Password) = ASU.To_String(Pswrd) then
            raise Shutdown;
        else
            T_IO.Put_Line(". IGNORED, incorrect password");
        end if;
    end Shutdown_Message;

    procedure Init_Message(P_Buffer: access LLU.Buffer_Type;
                           Mess: in CM.Message_Type;
                           Lists: in out Client_Type) is
        EP: LLU.End_Point_Type;
        Nick: ASU.Unbounded_String;
    begin
        begin
            Receive_Init(P_Buffer,EP,Nick);
            Save_Client(P_Buffer,EP,Lists,Nick,Mess);
            T_IO.Put_Line(" from " & ASU.To_String(Nick));
        exception
            when CC.Client_Collection_Error =>
                T_IO.Put_Line(" from " & ASU.To_String(Nick) & 
                              ". IGNORED, nick already used");
        end;
    end Init_Message;

    procedure Writer_Message(P_Buffer: access LLU.Buffer_Type;
                             Lists: in Client_Type) is
        EP: LLU.End_Point_Type;
        Message: ASU.Unbounded_String;
    begin
        Receive_Writer(P_Buffer,EP,Message);
        LLU.Reset(P_Buffer.all);
        Check_Client(P_Buffer,EP,Lists,Message);
    end Writer_Message;

    procedure Writer_Private_Message(P_Buffer: access LLU.Buffer_Type;
                                     Lists: in Client_Type) is
        EP_Src, EP_Dst: LLU.End_Point_Type;
        Nick_Src, Nick_Dst, Message: ASU.Unbounded_String;
    begin
        Receive_Writer_Private(P_Buffer,EP_Src,Nick_Dst,Message);
        LLU.Reset(P_Buffer.all);
        T_IO.Put(" from ");
        begin
            Nick_Src := CC.Search_Client(Lists(Writer),EP_Src);
            if ASU.To_String(Nick_Src) = "" then
                raise CC.Client_Collection_Error;
            end if;
            T_IO.Put(ASU.To_String(Nick_Src) & " to ");
            EP_Dst := CC.Search_Client_EP(Lists(Reader),Nick_Dst);
            T_IO.Put_Line(ASU.To_String(Nick_Dst));
            Send_Private_Server(P_Buffer,EP_Dst,Nick_Src,Message);
        exception
            when CC.Client_Collection_Error =>
                T_IO.Put_Line("unknown client. IGNORED");
        end;
    end Writer_Private_Message;

    procedure Receive_Any(P_Buffer: access LLU.Buffer_Type;
                          Lists: in out Client_Type;
                          Password: in ASU.Unbounded_String) is
        Mess: CM.Message_Type;
    begin
        Mess := CM.Message_Type'Input(P_Buffer);
        T_IO.Put(CM.Message_Type'Image(Mess) & " received");
        case Mess is
            when CM.InitW | CM.InitR =>
                Init_Message(P_Buffer,Mess,Lists);
            when CM.Writer =>
                Writer_Message(P_Buffer,Lists);
            when CM.Writer_Private =>
                Writer_Private_Message(P_Buffer,Lists);
            when CM.Collection_Request =>
                Collection_Message(P_Buffer,Lists(Writer),Password);
            when CM.Ban =>
                Ban_Message(P_Buffer,Lists(Writer),Password);
            when CM.Shutdown =>
                Shutdown_Message(P_Buffer,Password);
            when others =>
                null;
        end case;
    end Receive_Any;

    Password: ASU.Unbounded_String;
    Buffer: aliased LLU.Buffer_Type(1024);
    Server_EP: LLU.End_Point_Type;
    Lists: Client_Type;

begin

    case ACL.Argument_Count is
        when 2 =>
            Create_Server(Server_EP,Password);
            loop
                LLU.Reset(Buffer);
                LLU.Receive(Server_EP,Buffer'Access);
                Receive_Any(Buffer'Access,Lists,Password);
            end loop;
        when others =>
            raise Usage_Error;
    end case;

exception
    when Usage_Error | Constraint_Error =>
        T_IO.Put_Line("Usage error: " & Usage_Message);
        LLU.Finalize;
    when Shutdown =>
        LLU.Finalize;
    when Ex:others =>
        T_IO.Put_Line("Unexpected exception: " &
                             Ada.Exceptions.Exception_Name(Ex) & " in: " &
                             Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;

end Chat_Server;
