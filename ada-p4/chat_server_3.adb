with Ada.Text_IO, Ada.Command_Line, Ada.Exceptions, Ada.Characters.Handling;
with Lower_Layer_UDP, Server_Handler;

procedure Chat_Server_3 is

   -- gnatmake -I/usr/local/ll/lib chat_server_3.adb

   package T_IO renames Ada.Text_IO;
   package LLU  renames Lower_Layer_UDP;
   package ACL  renames Ada.Command_Line;
   package ACH  renames Ada.Characters.Handling;

   Usage_Error, Client_Max_Error : exception;
   
   procedure Get_Arguments (Port        : out Integer;
                            Max_Clients : out Integer) is
   
   begin
   
      Port        := Integer'Value(ACL.Argument(1));
      Max_Clients := Integer'Value(ACL.Argument(2));
      
      if Max_Clients < 2 or Max_Clients > 50 then
         -- Mantener el numero de clientes entre 2 y 50
         raise Client_Max_Error;
         
      end if;
   
   exception
   
      when Constraint_Error =>
         -- Si lo que introduce el usuario no es un entero
         raise Usage_Error;
   
   end Get_Arguments;
   
   procedure Bind_Server (EP   : out LLU.End_Point_Type;
                          Port : in Integer) is
   
   begin
      -- Para crear el servidor
      EP := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port);
      -- Atar handler del servidor
      LLU.Bind(EP, Server_Handler.Handler'access);
      
      T_IO.Put_Line("Host: " & LLU.Get_Host_Name);
      T_IO.Put_Line("IP: " & LLU.To_IP(LLU.Get_Host_Name));
      T_IO.Put_Line("Port:" & Integer'Image(Port));
   
   end Bind_Server;
   
   Port, Max_Clients : Integer;
   EP                : LLU.End_Point_Type;
   C                 : Character;
   
begin
   
   -- Sacamos las variables que necesitamos
   Get_Arguments(Port, Max_Clients);
   -- Atamos el servidor a un End Point
   Bind_Server(EP, Port);
   -- Bucle infinito
   loop
      -- Recoge la tecla que se pulse
      T_IO.Get_Immediate(C);
      T_IO.New_Line;
      
      if ACH.To_Lower(C) = 'l' then
         -- Muesta los clientes activos
         T_IO.Put_Line("ACTIVE CLIENTS");
         T_IO.Put_Line("--------------");
         Server_Handler.Show_Active;
      
      elsif ACH.To_Lower(C) = 'o' then
         -- Muesta los clientes antiguos
         T_IO.Put_Line(" OLD CLIENTS");
         T_IO.Put_Line("--------------");
         Server_Handler.Show_Old;
      
      else
      
         T_IO.Put_Line("Press 'l' or 'o'");
      
      end if;
      
      T_IO.New_Line;
   
   end loop;

exception

   when Usage_Error =>
      -- Error de uso
      T_IO.Put_Line ("usage: ./chat_server_3 <port> <num clients>");
      LLU.Finalize;
   
   when Client_Max_Error =>
      -- Error en numero de clientes
      T_IO.Put_Line ("number of clients must be between 2 and 50");
      LLU.Finalize;
   
   when Ex : others =>
      T_IO.Put_Line ("UNEXPECTED ERROR: " & Ada.Exceptions.Exception_Name(Ex) & " in: " & Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Chat_Server_3;
