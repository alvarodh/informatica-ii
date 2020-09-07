with Chat_Messages, Hash_Maps_G, Ordered_Maps_G;
with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Calendar, Ada.Command_Line;
with Gnat.Calendar, Gnat.Calendar.Time_IO;

package body Server_Handler is

--------------------------------------------------------------------------------------------
--                  VARIABLES Y PAQUETES PROPIOS DEL PROGRAMA                            --
--------------------------------------------------------------------------------------------
 
   package CM   renames Chat_Messages;
   package T_IO renames Ada.Text_IO;
   package ASU  renames Ada.Strings.Unbounded;
   package ACL  renames Ada.Command_Line;
   
   -- Para poder comparar tipos de mensajes --
   use type CM.Message_Type;
   
   -- Value para clientes activos --
   type AC_Value is record
      EP_Handler : LLU.End_Point_Type; -- EP donde recibe
      EP_Receive : LLU.End_Point_Type; -- EP donde envia
      Last_Mess  : Ada.Calendar.Time;  -- Cuando ha enviado el ultimo mensaje
   end record;

   -- Para poder comparar fechas --
   use type Ada.Calendar.Time;

   HASH_SIZE : constant := 17; -- Tamaño de la tabla Hash
   
   -- Nick que usará el servidor --
   Server_Nick : ASU.Unbounded_String := ASU.To_Unbounded_String("server");

   type Hash_Range is mod HASH_SIZE;

   function Character_Hash (Nick : ASU.Unbounded_String) return Hash_Range is
   
      Counter : Natural := 0;
      Length  : Natural;
      
   begin
   
      -- DEVUELVE EL VALOR HASH DE UN NICK --
      
      Length := ASU.Length (Nick); -- Longitud del Nick
      
      for I in 1..Length loop
         -- Sumamos el valor ASCII del caracter al contador --
         Counter := Counter + Character'Pos (ASU.Element(Nick, I));
         
      end loop;
      -- Sacamos el modulo del sumatorio de los valores del Unbounded_String --
      return Hash_Range'Mod(Counter);
      
   end Character_Hash;
   
   function Get_Max_Clients return Natural is
   
   begin
   
      return Natural'Value(ACL.Argument(2));
   
   end Get_Max_Clients;
   
   -- Número máximo de clientes --
   Max_Clients : Natural := Get_Max_Clients;
   
   -- Creación de paquetes genéricos para lstas de clientes --
   package Active_Clients is new Hash_Maps_G (Key_Type   => ASU.Unbounded_String,
                                              Value_Type => AC_Value,
                                              "="        => ASU."=",
                                              Hash_Range => Hash_Range,
                                              Hash       => Character_Hash,
                                              Max        => Max_Clients);

   package Old_Clients is new Ordered_Maps_G (Key_Type   => ASU.Unbounded_String,
                                              Value_Type => Ada.Calendar.Time,
                                              "="        => ASU."=",
                                              "<"        => ASU."<",
					                               Max        => 150);
   
   package AC renames Active_Clients;
   package OC renames Old_Clients;
   
   -- Variables globales del programa --
   Active_List : AC.Map;
   Old_List    : OC.Map;
   
--------------------------------------------------------------------------------------------
--                         FUNCIONES Y PROCEDIMIENTOS                                    --
--------------------------------------------------------------------------------------------

   function Client_Address (Address : ASU.Unbounded_String) return ASU.Unbounded_String is
      -- ENTRADA : UNBOUNDED_STRING del END_POINT
      -- SALIDA  : UNBOUNDED_STRING → IP:PUERTO 
      Posicion      : Natural;
      Aux, Port, IP : ASU.Unbounded_String;
      
   begin
    
       Posicion := ASU.Index (Address, ":" );
       Aux      := ASU.Tail (Address, ASU.Length(Address) - (Posicion + 1));
       Posicion := ASU.Index (Aux, ",");
       IP       := ASU.Head (Aux, Posicion - 1);
       Posicion := ASU.Index (Aux, ":");
       Port     := ASU.Tail (Aux, ASU.Length(Aux) - Posicion);
       Posicion := ASU.Index (Port, " ");
       Port     := ASU.Tail (Port, ASU.Length(Port) - (Posicion + 1));
        
       return ASU.To_Unbounded_String (ASU.To_String(IP) & ":" & ASU.To_String(Port));
        
   end Client_Address;

   function Time_Image (T : Ada.Calendar.Time) return String is
   
   begin
   
      return Gnat.Calendar.Time_IO.Image (T, "%d-%b-%y %T.%i");
      
   end Time_Image;

   procedure Send_To_All (Nick     : in ASU.Unbounded_String;
                          P_Buffer : access LLU.Buffer_Type) is
      
      C : AC.Cursor := AC.First(Active_List);
      E : AC.Element_Type;
   
   begin
   
      loop
         -- Obtenemos elemento → (key, value) --
         E := AC.Element(C);
         
         if not ASU."="(E.Key, Nick) then
            -- Si no es el cliente nuevo --
            LLU.Send(E.Value.EP_Handler, P_Buffer);
         
         end if;
         -- Pasamos al siguiente cliente --
         AC.Next(C);
         
         exit when not AC.Has_Element(C);
      
      end loop;
   
   exception
   
      when AC.Full_Map | AC.No_Element =>
         -- Cuando el mapa este lleno o no haya mas clientes
         null;
   
   end Send_To_All;
   
   procedure Server_Mess (Nick, Request : in ASU.Unbounded_String;
                          Nick_src      : in ASU.Unbounded_String;
                          P_Buffer      : access LLU.Buffer_Type) is
   
   begin
   
      LLU.Reset(P_Buffer.all);
      -- CM.SERVER | NICK | RESPUESTA --
      CM.Message_Type'Output(P_Buffer, CM.Server);
      ASU.Unbounded_String'Output(P_Buffer, Nick);
      ASU.Unbounded_String'Output(P_Buffer, Request);
      
      Send_To_All(Nick_src, P_Buffer);
   
   end Server_Mess;
   
   procedure New_User (Nick     : in ASU.Unbounded_String;
                       P_Buffer : access LLU.Buffer_Type) is
   
      Reply : ASU.Unbounded_String;
   
   begin
      
      -- Crear mensaje de tipo Server para informar de la entrada de un cliente nuevo --
      Reply := ASU.To_Unbounded_String(ASU.To_String(Nick) & " joins to the chat");
      Server_Mess(Server_Nick, Reply, Nick, P_Buffer);
   
   end New_User;
   
   procedure User_Leave (Nick     : in ASU.Unbounded_String;
                         P_Buffer : access LLU.Buffer_Type) is
   
      Reply : ASU.Unbounded_String;
   
   begin
   
      -- Crear mensaje de tipo Server para informar de la marcha de un cliente --
      Reply := ASU.To_Unbounded_String(ASU.To_String(Nick) & " leaves the chat");
      Server_Mess(Server_Nick, Reply, Nick, P_Buffer);
   
   end User_Leave;
   
   procedure Send_Welcome (Success  : in Boolean;
                           EP_Dest  : in LLU.End_Point_Type;
                           P_Buffer : access LLU.Buffer_Type) is

   begin
   
      LLU.Reset(P_Buffer.all);
      -- CM.WELCOME | ACCEPTED --
      CM.Message_Type'Output(P_Buffer, CM.Welcome);
      Boolean'Output(P_Buffer, Success);
      
      LLU.Send(EP_Dest, P_Buffer);

   end Send_Welcome;
   
   procedure Send_Ban (Nick     : in ASU.Unbounded_String;
                       P_Buffer : access LLU.Buffer_Type) is
   
      Reply : ASU.Unbounded_String;
   
   begin
   
      -- Crear mensaje de tipo Server para informar del baneo de un cliente --
      Reply := ASU.To_Unbounded_String(ASU.To_String(Nick) & " banned for being idle too long");
      Server_Mess(Server_Nick, Reply, Nick, P_Buffer);
   
   end Send_Ban;
   
   function Search_Oldest return ASU.Unbounded_String is
   
      C    : AC.Cursor := AC.First(Active_List);
      E    : AC.Element_Type;
      Nick : ASU.Unbounded_String := ASU.Null_Unbounded_String;
      Date : Ada.Calendar.Time;
   
   begin
      -- Sacamos los valores del primer cliente --
      E    := AC.Element(C);
      Nick := E.Key;
      Date := E.Value.Last_Mess;
   
      loop
         -- Sacamos elemento → (key, value) --
         E := AC.Element(C);
         
         if E.Value.Last_Mess < Date then
            -- Si la fecha es menor, ha pasado más tiempo
            Nick := E.Key;
            Date := E.Value.Last_Mess;
         
         end if;
         -- Pasamos al siguiente --
         AC.Next(C);
         -- Comprobamos si es el último --
         exit when not AC.Has_Element(C);
      
      end loop;
      
      return Nick;
   
   exception
   
      when AC.Full_Map | AC.No_Element =>
         -- Lista llena o elemento vaciío --
         return Nick;
   
   end Search_Oldest;
   
   procedure Ban_Oldest (P_Buffer : access LLU.Buffer_Type) is
   
      Nick    : ASU.Unbounded_String;
      Success : Boolean;
   
   begin
      
      Nick := Search_Oldest; -- Nos devuelve el nick del cliente mas antiguo
      
      Send_Ban(Nick, P_Buffer);
      AC.Delete(Active_List, Nick, Success);      -- Borramos de clientes activos
      OC.Put(Old_List, Nick, Ada.Calendar.Clock); -- Añadimos a clientes antiguos
   
   end Ban_Oldest;
   
   procedure Add_Client (Nick     : in ASU.Unbounded_String;
                         Value    : in AC_Value;
                         P_Buffer : access LLU.Buffer_Type) is
   
      Success : Boolean;
   
   begin
   
      AC.Put(Active_List, Nick, Value);   -- Añadimos al cliente siempre
      OC.Delete(Old_List, Nick, Success); -- Lo borramos de la lista de antiguos
      New_User(Nick, P_Buffer);
   
   end Add_Client;
   
   procedure Receive_Init (P_Buffer : access LLU.Buffer_Type) is
   
      Value, Aux : AC_Value;
      Nick       : ASU.Unbounded_String;
      Success    : Boolean;
   
   begin
      
      -- CM.INIT | EP_RECEIVE | EP_HANDLER | NICK --
      Value.EP_Receive := LLU.End_Point_Type'Input(P_Buffer);
      Value.EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
      Value.Last_Mess  := Ada.Calendar.Clock; -- Nos devuelve la hora
      Nick             := ASU.Unbounded_String'Input(P_Buffer);
      -- Miramos si está ya registrado --
      AC.Get(Active_List, Nick, Aux, Success);
      T_IO.Put(ASU.To_String(Nick));
      
      if not Success then
         -- Si no está registrado, le añadimos --
         T_IO.Put_Line(": ACCEPTED");
         Add_Client(Nick, Value, P_Buffer);
      
      else
      
         T_IO.Put_Line(": IGNORED, nick already in use");
      
      end if;
      -- Enviar mensaje de aceptacion --
      Send_Welcome(not Success, Value.EP_Receive, P_Buffer);
   
   exception
   
      when AC.Full_Map =>
         -- Lista de clientes activos completa --
         Ban_Oldest(P_Buffer);                           -- Banear
         Add_Client(Nick, Value, P_Buffer);              -- Añadir
         Send_Welcome(True, Value.EP_Receive, P_Buffer); -- Enviar mensaje de aceptacion
   
   end Receive_Init;
   
   procedure Receive_Writer (P_Buffer : access LLU.Buffer_Type) is
   
      EP            : LLU.End_Point_Type;
      Aux           : AC_Value;
      Nick, Request : ASU.Unbounded_String;
      Success       : Boolean;
   
   begin
      -- CM.WRITER | EP_HANDLER | NICK | RESPUESTA --
      EP      := LLU.End_Point_Type'Input(P_Buffer);
      Nick    := ASU.Unbounded_String'Input(P_Buffer);
      Request := ASU.Unbounded_String'Input(P_Buffer);
      -- Miramos si está registrado
      AC.Get(Active_List, Nick, Aux, Success);
      
      if Success then
         -- Si está registrado, actualizamos value, enviamos a todos
         T_IO.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Request));
         Aux.Last_Mess := Ada.Calendar.Clock;
         AC.Put(Active_List, Nick, Aux);
         Server_Mess(Nick, Request, Nick, P_Buffer);
      
      else
         -- No esta registrado, no hacemos nada
         T_IO.Put_Line("unknown client: IGNORED");
      
      end if;
   
   end Receive_Writer;
   
   procedure Receive_Logout (P_Buffer : access LLU.Buffer_Type) is
   
      EP      : LLU.End_Point_Type;
      Value   : AC_Value;
      Nick    : ASU.Unbounded_String;
      Success : Boolean;
   
   begin
      -- CM.LOGOUT | EP_HANDLER | NICK --
      EP   := LLU.End_Point_Type'Input(P_Buffer);
      Nick := ASU.Unbounded_String'Input(P_Buffer);
      -- Miramos si está registrado
      AC.Get(Active_List, Nick, Value, Success);
      
      if Success then
         -- Si está registrado, le damos de baja
         User_Leave(Nick, P_Buffer);
         AC.Delete(Active_List, Nick, Success);
         OC.Put(Old_List, Nick, Ada.Calendar.Clock);
         T_IO.Put_Line(ASU.To_String(Nick) & ": leaves the chat");
      
      else
         -- Si no está registrado, no hacemo nada
         T_IO.Put_Line("unknown client: IGNORED");
      
      end if;
   
   end Receive_Logout;

   procedure Handler (From     : in LLU.End_Point_Type;
                      To       : in LLU.End_Point_Type;
                      P_Buffer : access LLU.Buffer_Type) is

      Mess : CM.Message_Type;

   begin

      -- Sacamos el tipo de mensaje que nos ha llegado
      Mess := CM.Message_Type'Input (P_Buffer);
      
      T_IO.Put(CM.Message_Type'Image(Mess) & " received from ");
      
      case Mess is
      
         when CM.Init =>
            -- Mensaje de tipo CM.INIT --
            Receive_Init(P_Buffer);
            
         when CM.Writer =>
            -- Mensaje de tipo CM.WRITER --
            Receive_Writer(P_Buffer);
            
         when CM.Logout =>
            -- Mensaje de tipo CM.LOGOUT --
            Receive_Logout(P_Buffer);
            
         when others =>
            -- Mensaje de cualquier otro tipo, no hacemos nada
            null;
            
      end case;
      
      LLU.Reset(P_Buffer.all);

   end Handler;
   
--------------------------------------------------------------------------------------------
--         PROCEDIMIENTOS PARA MOSTRAR LISTAS CLIENTES ACTIVOS Y ANTIGUOS                --
--------------------------------------------------------------------------------------------

   procedure Show_Active is
   
      C        : AC.Cursor := AC.First(Active_List);
      E        : AC.Element_Type;
      EP_Image : ASU.Unbounded_String;
   
   begin
   
      loop
         
         E        := AC.Element(C);
         EP_Image := ASU.To_Unbounded_String(LLU.Image(E.Value.EP_Handler));
         
         T_IO.Put_Line(ASU.To_String(E.Key) & ", Address: " &
                       ASU.To_String(Client_Address(EP_Image)) & ", Last message: " &
                       Time_Image(E.Value.Last_Mess));
         
         AC.Next(C);
         
         exit when not AC.Has_Element(C);
      
      end loop;
   
   exception
   
      when AC.Full_Map | AC.No_Element =>
      
         null;
   
   end Show_Active;

   procedure Show_Old is

      C : OC.Cursor := OC.First(Old_List);
      E : OC.Element_Type;
   
   begin
   
      loop
         
         E := OC.Element(C);
         
         T_IO.Put_Line(ASU.To_String(E.Key) & " " & Time_Image(E.Value));
         
         OC.Next(C);
         
         exit when not OC.Has_Element(C);
      
      end loop;
   
   exception
   
      when OC.Full_Map | OC.No_Element =>
      
         null;

   end Show_Old;

end Server_Handler;
