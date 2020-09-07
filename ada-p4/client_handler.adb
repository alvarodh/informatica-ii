with Ada.Text_IO, Ada.Strings.Unbounded;
with Chat_Messages;

package body Client_Handler is
   
   package T_IO renames Ada.Text_IO;
   package ASU  renames Ada.Strings.Unbounded;
   package CM   renames Chat_Messages;

   use type CM.Message_Type;

   procedure Handler (From     : in LLU.End_Point_Type;
                      To       : in LLU.End_Point_Type;
                      P_Buffer : access LLU.Buffer_Type) is

      Mess_Type   : CM.Message_Type;
      Nick, Reply : ASU.Unbounded_String;

   begin
      
      -- Tipo de mensaje --
      Mess_Type := CM.Message_Type'Input(P_Buffer);

      if Mess_Type = CM.Server then
         -- CM.SERVER | NICK | RESPUESTA --
         Nick  := ASU.Unbounded_String'Input(P_Buffer);
         Reply := ASU.Unbounded_String'Input(P_Buffer);
      
         T_IO.New_Line;
         T_IO.Put_Line (ASU.To_String(Nick) & ": " & ASU.To_String(Reply));
         T_IO.Put (">> ");
         
      end if;
      
      LLU.Reset(P_Buffer.all);

   end Handler;

end Client_Handler;
