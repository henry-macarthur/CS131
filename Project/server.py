import asyncio 
import sys 
import argparse 
import time
import aiohttp 
import re
import json


localhost = '127.0.0.1'

client_data = {}

name_to_port = {
        "Riley":    15445,
        "Jaquez":   15446,
        "Juzang":   15447,
        "Campbell": 15448,
        "Bernard":  15449 
    }

communicates_with = {
    "Riley":    ["Jaquez", "Juzang"],
    "Bernard":  ["Jaquez", "Juzang", "Campbell"],
    "Juzang":   ["Campbell", "Riley", "Bernard"],
    "Jaquez":   ["Riley", "Bernard"],
    "Campbell": ["Juzang", "Bernard"],
    
}

def handle_invalid(message):
    return "? "+ message

def check_id(id):
    return not any(ext in id for ext in [' ', '\t', '\n', '\r', '\f', '\v'])

def handle_time_difference(server_time, user_time):

    if server_time > user_time: 
        return "+" +  str(server_time - user_time)
    else: 
        return "-" + str(user_time - server_time)
    
def get_coords(coord):

    if(len(coord) < 4):
        return None

    num_signs = 0;
    first_sign = -1;
    second_sign = -1;

    #get the index of each coordinate
    for i in range(len(coord)):
        if(coord[i] == '+' or coord[i] == '-'):
            num_signs = num_signs + 1;
            if first_sign == -1:
                first_sign = i
            elif second_sign == -1: 
                second_sign = i
            else:
                return None

    if second_sign == -1 or first_sign != 0:
        return None

    return (coord[0: second_sign], coord[second_sign:])

async def send_SERVER(client_id):
    #propagate the information to each server
    for server in (communicates_with[name_in_use]):
        #grab all of the relevant information
        cur_port = name_to_port[server]
        server_t = client_data[client_id][0]
        msg_time = client_data[client_id][1]
        lat = client_data[client_id][2]
        lon = client_data[client_id][3]

        message = "SERVERMSG " + client_data[client_id][4] + " " + client_data[client_id][5][1] + " " + lat + " " + lon + " " + str(server_t) + " " + str(msg_time)
        #send msg type, id, lat, lon, server time of msg, time of msg being sent 
        #open the connection to the desired server
        try:
            reader,writer1 = await asyncio.open_connection(localhost, name_to_port[server])
            file_log.write("connected to server " + str(name_to_port[server]) + "\n")
            writer1.write(message.encode())
            print_sent(message, name_to_port[server])
            await writer1.drain()
            writer1.close()
            await writer1.wait_closed()
            file_log.write("disconnected to server " + str(name_to_port[server]) + "\n")
            
        except:

            file_log.write("Error connecting to server " + str(name_to_port[server]) + "\n")
            #do something here for logging 

async def handle_SERVER(message, message_list):
    cur_id = message_list[2]

    #if we dont have the latest update, we update and propogate it
    if cur_id not in client_data or client_data[cur_id][0] < float(message_list[5]):
        #print (message)
        current_data = [
            float(message_list[5]),
            float(message_list[6]),
            message_list[3],
            message_list[4],
            message_list[1],
            ["IMAT", cur_id, (message_list[3]+message_list[4]), message_list[6]]
        ]

        client_data[cur_id] = current_data
        asyncio.ensure_future(send_SERVER(cur_id))
    else:
        pass 
        #this means that we already have the most up to date copy


async def handle_IMAT(message, message_list, time):
    #get_coords(message_list[1]) is None
    #make sure the id is valid
    if not check_id(message_list[1]) or get_coords(message_list[2]) is None: 
        return handle_invalid(message)
    
    time_diff = handle_time_difference(time, float(message_list[3]))
    lat, lon = get_coords(message_list[2])  #might use this later
    #might need to store more information
    current_data = [
        time,                       #0
        float(message_list[3]),     #1
        lat,                        #2
        lon,                        #3
        name_in_use,                #4
        message_list                #5
    ]

    #map the current client id to a list of relevant information
    client_data[message_list[1]] = current_data
    
    response_message = "AT " + name_in_use + " " + time_diff + " " + ' '.join(message_list[1:])

    asyncio.ensure_future(send_SERVER(message_list[1]))

    return response_message


async def handle_WHATSAT(message, message_list):
    
    client_id = message_list[1]
    radius = message_list[2]
    bound = int(message_list[3])

    if(int(radius) > 50 or int(bound) > 20 or not check_id(client_id) or (client_id not in client_data)):
        return handle_invalid(message)
    
    current_msg = client_data[client_id]
    
    radius_m = int(radius) * 1000
    
    #start working on the http request

    request_url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=" + current_msg[2] + "," + current_msg[3] + "&radius=" + str(radius_m) + "&key=" + key
    async with aiohttp.ClientSession(
        connector=aiohttp.TCPConnector(
            ssl=False,
        ),
    ) as session: 
        async with session.get(request_url) as response: 
            #re.sub('\n\n+', '\n', response)
            response_message = await response.json()
    # we can do some 404 checking later
    response_message['results'] = response_message['results'][0:bound] # is this inclusive?
    response_str = json.dumps(response_message, indent=4).rstrip('\n')
    re.sub('\n\n+', '\n', response_str)

    output_response = "AT " + current_msg[4] + " " + handle_time_difference(current_msg[0], current_msg[1]) + " " + ' '.join(current_msg[5][1:]) + "\n"
    output_response += response_str + "\n\n"
    #do  i add another newline?


    return output_response
    
def print_sent(msg, port):
    file_log.write("TO " + str(port) + " SENT: " + msg + "\n")

def print_rec(msg, port):
    file_log.write("FROM " + str(port) + " RECV : " + msg + "\n")

async def handle_message(writer, message_list, message, time, addr): 

    print_rec(message, addr)
    server_response = ""
    if len(message_list) < 4:
        server_response = handle_invalid(message)
    
    message_type = message_list[0]

    if message_type == 'IAMAT':
        #need to check to make sure this is valid
        if len(message_list) == 4: 
            server_response = await handle_IMAT(message, message_list, time)
    elif message_type == 'WHATSAT':
        if len(message_list) == 4:
            server_response = await handle_WHATSAT(message, message_list)
    elif message_type == "SERVERMSG":
            await handle_SERVER(message, message_list)
    else: 
            server_response = handle_invalid(message)

    #might want to do some condition checking here 
    if message_type != "SERVERMSG":
        file_log.write(server_response)
        server_message = server_response.encode()
        writer.write(server_message)
        print_sent(server_response, str(addr))
        await writer.drain()
        writer.close()

    
async def server_helper(reader, writer):
    data = await reader.read(1000)
    time_received = time.time() #get the time the message was recieved 
    message = data.decode()
    updated_message = message.strip()
    message_items = updated_message.split(" ")  #get a list of all of the inputs
    addr = writer.get_extra_info('peername')
    await handle_message(writer, message_items, updated_message, time_received, str(addr))

def main():

    parser = argparse.ArgumentParser()
    parser.add_argument("server_name", type=str, help="Enter a server name!")   #make the user specify a server name
    args = parser.parse_args()

    if args.server_name not in name_to_port:
        print("ENTER A VALID SERVER NAME! \n")
        sys.exit(1)
    
    global name_in_use
    name_in_use = args.server_name

    global file_log
    nm = name_in_use + ".txt"
    file_log = open(nm, "w+")
    file_log.truncate(0)
    #file_log.write("adasfa")
    #file_log.truncate(0)

    port_number = name_to_port[args.server_name]

    loop = asyncio.get_event_loop()
    coro = asyncio.start_server(server_helper, localhost, port_number, loop=loop)
    server = loop.run_until_complete(coro)
    
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()
    file_log.close()

if __name__ == '__main__':
    main()
