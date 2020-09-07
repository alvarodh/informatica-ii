with Ada.Text_IO, Ada.Command_Line, Ada.Strings.Unbounded, Ada.Exceptions, Ada.Characters.Handling;
with Lower_Layer_UDP, Chat_Messages, Client_Handler;

procedure Chat_Client_3 is

   package T_IO renames Ada.Text_IO;
   package ASU  renames Ada.Strings.Unbounded;
   package LLU  renames Lower_Layer_UDP;
   package ACL  renames Ada.Command_Line;
   package ACH  renames Ada.Characters.Handling;
   package CM   renames Chat_Messages;

   use type CM.Message_Type;
   
   -- Tipo del cliente --
   type Client_Type is record
      Nick    : ASU.Unbounded_String;
      Receive : LLU.End_Point_Type;
      Handler : LLU.End_Point_Type;
   end record;
   
   -- Excepciones del programa --
   Usage_Error, Nick_Error, Server_Unreachable, Welcome_Error : exception;
   
   procedure Get_Arguments (Host : out ASU.Unbounded_String;
                            Port : out Integer;
                            Nick : out ASU.Unbounded_String) is

   begin
   
      Host := ASU.To_Unbounded_String (ACL.Argument(1)); -- Host del server
      Port := Integer'Value (ACL.Argument(2));           -- Puerto del server
      Nick := ASU.To_Unbounded_String (ACL.Argument(3)); -- Nick del usuario
      
      if ACH.To_Lower (ASU.To_String(Nick)) = "server" then
         -- Este nick no esta permitido --
         raise Nick_Error;
         
      end if;
   
   exception
   
      when Constraint_Error =>
         -- Nos falta algun argumento --
         raise Usage_Error;
   
   end Get_Arguments;
   
   procedure Tie_Server (EP   : out LLU.End_Point_Type;
                         Host : in ASU.Unbounded_String;
                         Port : in Integer) is
   
   begin
      -- Crea el EP en el que esta el servidor --
      EP := LLU.Build(LLU.To_IP(ASU.To_String(Host)), Port);
      
   end Tie_Server;
   
   procedure Bind_Client (Nick   : in ASU.Unbounded_String;
                          Client : out Client_Type) is
   
      EP_Receive, EP_Handler : LLU.End_Point_Type;
   
   begin
   
      LLU.Bind_Any (EP_Handler, Client_Handler.Handler'Access); -- Ata en un EP cualquiera
      LLU.Bind_Any (EP_Receive);                                -- Ata en un EP cualquiera
      -- Creamos el cliente --
      Client.Nick    := Nick;
      Client.Handler := EP_Handler;
      Client.Receive := EP_Receive;
   
   end Bind_Client;
   
   procedure Send_Init (Client   : in Client_Type;
                        EP_Dest  : in LLU.End_Point_Type;
                        P_Buffer : access LLU.Buffer_Type) is
   
   begin
   
      LLU.Reset(P_Buffer.all);
      -- CM.INIT | EP_RECEIVE | EP_HANDLER | NICK --
      CM.Message_Type'Output (P_Buffer, CM.Init);
      LLU.End_Point_Type'Output (P_Buffer, Client.Receive);
      LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
      ASU.Unbounded_String'Output (P_Buffer, Client.Nick);
      -- Enviamos al servidor --
      LLU.Send (EP_Dest, P_Buffer);
   
   end Send_Init;
   
   procedure Receive_Welcome (Client : in Client_Type;
                              P_Buffer : access LLU.Buffer_Type) is
   
      Expired, Accepted : Boolean;
      Mess              : CM.Message_Type;
   
   begin
   
      LLU.Reset(P_Buffer.all);
      -- Esperar a recibir WELCOME --
      LLU.Receive (Client.Receive, P_Buffer, 10.0, Expired);
      
      if Expired then
         -- El servidor no ha respondido --
         raise Server_Unreachable;
      
      else
         -- CM.WELCOME | ACCEPTED --
         Mess     := CM.Message_Type'Input(P_Buffer);
         Accepted := Boolean'Input(P_Buffer);
         
         T_IO.Put("Mini-Chat v3.0: ");
         
         if Accepted then
            -- Si se nos ha aceptado, seguimos con la ejecución --
            T_IO.Put_Line("Welcome " & ASU.To_String(Client.Nick));
            T_IO.Put(">> ");
         
         else
            -- Si no se nos ha aceptado, cerramos el programa --
            raise Welcome_Error;
         
         end if;
      
      end if;
   
   end Receive_Welcome;
   
   procedure Send_Writer (Client   : in Client_Type;
                          Request  : in ASU.Unbounded_String;
                          EP_Dest  : in LLU.End_Point_Type;
                          P_Buffer : access LLU.Buffer_Type) is
   
   begin
   
      LLU.Reset(P_Buffer.all);
      -- CM.WRITER | EP_HANDLER | NICK | RESPUESTA --
      CM.Message_Type'Output (P_Buffer, CM.Writer);
      LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
      ASU.Unbounded_String'Output (P_Buffer, Client.Nick);
      ASU.Unbounded_String'Output (P_Buffer, Request);
      
      LLU.Send (EP_Dest, P_Buffer);
      T_IO.Put (">> ");
   
   end Send_Writer;
   
   procedure Send_Logout(Client   : in Client_Type;
                         EP_Dest  : in LLU.End_Point_Type;
                         P_Buffer : access LLU.Buffer_Type) is
   
   begin
   
      LLU.Reset(P_Buffer.all);
      -- CM.LOGOUT | EP_HANDLER | NICK --
      CM.Message_Type'Output (P_Buffer, CM.Logout);
      LLU.End_Point_Type'Output (P_Buffer, Client.Handler);
      ASU.Unbounded_String'Output (P_Buffer, Client.Nick);
      
      LLU.Send (EP_Dest, P_Buffer);
   
   end Send_Logout;
   
   Host, Nick, Request : ASU.Unbounded_String;
   Port                : Integer;
   Client              : Client_Type;
   Server_EP           : LLU.End_Point_Type;
   Buffer              : aliased LLU.Buffer_Type(1024);
   
begin

   Get_Arguments(Host, Port, Nick);   -- Sacamos los argumentos
   Tie_Server(Server_EP, Host, Port); -- Atamos el servidor a un EP
   Bind_Client(Nick, Client);         -- Crear el cliente
   
   Send_Init(Client, Server_EP, Buffer'Access); -- Enviar mensaje INIT
   Receive_Welcome(Client, Buffer'Access);      -- Esperar a recibir WELCOME
   
   loop
      -- Recoger el mensaje que escriba el cliente --
      Request := ASU.To_Unbounded_String(T_IO.Get_Line);
      
      if ACH.To_Lower(ASU.To_String(Request)) = ".quit" then
         -- Enviamos un mensaje de tipo LOGOUT --
         Send_Logout(Client, Server_EP, Buffer'Access);
         exit;
      
      else
         -- Enviamos un mensaje de tipo WRITER --
         Send_Writer(Client, Request, Server_EP, Buffer'Access);
      
      end if;
   
   end loop;
   
   LLU.Finalize;

exception

   when Usage_Error =>
      -- Error de uso --
      T_IO.Put_Line("usage error: ./chat_client_3 <Host><Port><Nick>");
      LLU.Finalize;
   
   when Nick_Error =>
      -- Nick no es valido --
      T_IO.Put_Line("nick not valid, try again");
      LLU.Finalize;
   
   when Server_Unreachable =>
      -- No se ha obtenido respuesta del servidor --
      T_IO.Put_Line("server unreachable");
      LLU.Finalize;
   
   when Welcome_Error =>
      -- No aceptado --
      T_IO.Put_Line("IGNORED new user " & ASU.To_String(Nick) & ", nick already in use");
      LLU.Finalize;

   when Ex : others =>
      T_IO.Put_Line ("UNEXPECTED ERROR: " & Ada.Exceptions.Exception_Name(Ex) & "en: " & Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Chat_Client_3;
