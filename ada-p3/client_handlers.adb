with Ada.Text_IO, Ada.Strings.Unbounded;
with Chat_Messages;

package body Client_Handlers is

    package T_IO renames           Ada.Text_IO;
    package  ASU renames Ada.Strings.Unbounded;
    package   CM renames         Chat_Messages;

    use type CM.Message_Type;

    Not_Server_Message: Exception;

    procedure Receive_Server (P_Buffer    : access   LLU.Buffer_Type;
                              Nick, Reply : out ASU.Unbounded_String) is
        Mess : CM.Message_Type;
    begin

        Mess  := CM.Message_Type'Input (P_Buffer);

        if Mess /= CM.Server then
            raise Not_Server_Message;
        end if;

        Nick  := ASU.Unbounded_String'Input (P_Buffer);
        Reply := ASU.Unbounded_String'Input (P_Buffer);

    end Receive_Server;

    procedure Handler (From     : in  LLU.End_Point_Type;
                       To       : in  LLU.End_Point_Type;
                       P_Buffer : access LLU.Buffer_Type) is
        Nick, Reply : ASU.Unbounded_String;
    begin

        begin

            Receive_Server (P_Buffer, Nick, Reply);

            T_IO.New_Line;
            T_IO.Put_Line (ASU.To_String (Nick) & ": " & ASU.To_String (Reply));
            T_IO.Put (">> ");

            LLU.Reset (P_Buffer.all);

        exception

            when Not_Server_Message =>
                LLU.Reset(P_Buffer.all);

        end;

    end Handler;

end Client_Handlers;
