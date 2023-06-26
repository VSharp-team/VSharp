import logging

import httplib2

from common.constants import BrokerLinks

from .classes import Agent2ResultsOnMaps


class Broker:
    def aquire_ws(self):
        while True:
            response, content = httplib2.Http().request(BrokerLinks.BROKER_LINK_GET)
            logging.info(f"Status: {response.status}")

            my_websocket = content.decode("utf-8")
            if my_websocket == "":
                logging.warning(f"all sockets are in use")
                continue
            logging.info(f"aquired ws: {my_websocket}")
            return my_websocket

    def return_ws(self, socket):
        logging.info(f"returning: {socket}")

        response, content = httplib2.Http().request(
            BrokerLinks.BROKER_LINK_POST,
            method="POST",
            body=socket,
        )

        if response.status == 200:
            logging.info(f"{socket} is returned")
        else:
            logging.error(f"{response.status} on returning {socket}")
            raise RuntimeError(f"Not ok response: {response.status}")

    def send_game_results(self, data: Agent2ResultsOnMaps):
        response, content = httplib2.Http().request(
            BrokerLinks.BROKER_LINK_SEND_RES,
            method="POST",
            body=data.to_json(),
        )

        if response.status == 200:
            logging.info(f"map2result was sent")
        else:
            logging.error(f"{response.status} on sending map2result: {data}")
            raise RuntimeError(f"Not ok response: {response.status}")

    def recv_game_result_list(self) -> str:
        response, content = httplib2.Http().request(BrokerLinks.BROKER_LINK_RECV_RES)
        logging.info(f"Status: {response.status}")

        games_data = content.decode("utf-8")
        logging.info(f"Aquired games data")
        return games_data
