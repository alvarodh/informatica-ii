with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Command_Line;
with Lower_Layer_UDP, Chat_Messages, Server_Handlers;

procedure Chat_Server_2 is

    package T_IO renames           Ada.Text_IO;
    package  ASU renames Ada.Strings.Unbounded;
    package  ACL renames      Ada.Command_Line;
    package  LLU renames       Lower_Layer_UDP;
    package   CM renames         Chat_Messages;

    Min_Clients : constant Natural := 2;
    Max_Clients : constant Natural := 50;

    Usage_Error, Client_Number_Error : Exception;
    Usage_Message : String := "./chat_server_2 <port> <num_clients>";

    type T_Server is record
        Receive : LLU.End_Point_Type;
        Handler : LLU.End_Point_Type;
    end record;

    function Get_Address (IP: ASU.Unbounded_String;
                          Port: Natural) return ASU.Unbounded_String is
        Port_Image, Address : ASU.Unbounded_String;
    begin

        Port_Image := ASU.To_Unbounded_String (Natural'Image(Port));
        ASU.Tail (Port_Image, ASU.Length (Port_Image) - ASU.Index (Port_Image, " "));

        Address := ASU.To_Unbounded_String (ASU.To_String (IP) & ":" &
                                            ASU.To_String (Port_Image));

        return Address;

    end Get_Address;

    procedure Create_Server (Server      : out T_Server;
                             Num_Clients : out  Natural) is
        Port              :              Natural;
        Host, IP, Address : ASU.Unbounded_String;
    begin

        Host        := ASU.To_Unbounded_String (LLU.Get_Host_Name);
        IP          := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String (Host)));
        Port        := Natural'Value (ACL.Argument(1));
        Num_Clients := Natural'Value (ACL.Argument(2));
        Address     := Get_Address (IP, Port);

        if Num_Clients < Min_Clients or Num_Clients > Max_Clients then
            raise Client_Number_Error;
        end if;

        Server.Handler := LLU.Build (ASU.To_String (IP), Port);

        LLU.Bind (Server.Handler, Server_Handlers.Handler'Access);
        LLU.Bind_Any (Server.Receive);
        T_IO.Put_Line ("Host name: " & ASU.to_String (Host));
        T_IO.Put_Line ("Address: " & ASU.To_String (Address));

    end Create_Server;
        
    Server      :  T_Server;
    C           : Character;
    Num_Clients :   Natural;

begin

    if ACL.Argument_Count /= 2 then
        raise Usage_Error;
    end if;

    Create_Server (Server, Num_Clients);

    loop
        T_IO.Get_Immediate (C);
        T_IO.New_Line;

        case C is
            when 'o' | 'O' =>
                T_IO.Put_Line ("           OLD CLIENTS");
                T_IO.Put_Line ("==================================");

                Server_Handlers.Print_Old_Clients;

            when 'l' | 'L' =>
                T_IO.Put_Line ("          ACTIVE CLIENTS");
                T_IO.Put_Line ("==================================");

                Server_Handlers.Print_Active_Clients;

            when others =>
                T_IO.Put_Line ("No option for " & C);
                T_IO.Put_Line ("Try with 'o' or 'l'");

        end case;

        T_IO.New_Line;
    end loop;

exception

    when Usage_Error | Constraint_Error =>
        T_IO.Put_Line ("Usage error: " & Usage_Message);
        LLU.Finalize;

    when Client_Number_Error =>
        T_IO.Put_Line ("The number of clients must be between" &
                       Natural'Image(Min_Clients) & " and" & Natural'Image(Max_Clients));
        LLU.Finalize;

end Chat_Server_2;
