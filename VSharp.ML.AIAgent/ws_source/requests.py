import logging

import httplib2

from common.constants import ResultsHandlerLinks, WebsocketSourceLinks

from .classes import Agent2ResultsOnMaps


def aquire_ws():
    while True:
        response, content = httplib2.Http().request(WebsocketSourceLinks.GET_WS)
        aquired_ws = content.decode("utf-8")
        if aquired_ws == "":
            logging.warning(f"all sockets are in use")
            continue
        logging.info(f"aquired ws: {aquired_ws}")
        return aquired_ws


def return_ws(websocket):
    logging.info(f"returning: {websocket}")

    response, content = httplib2.Http().request(
        WebsocketSourceLinks.POST_WS,
        method="POST",
        body=websocket,
    )

    if response.status == 200:
        logging.info(f"{websocket} is returned")
    else:
        logging.error(f"{response.status} on returning {websocket}")
        raise RuntimeError(f"Not ok response: {response.status}")


def send_game_results(data: Agent2ResultsOnMaps):
    response, content = httplib2.Http().request(
        ResultsHandlerLinks.POST_RES,
        method="POST",
        body=data.to_json(),
    )

    if response.status == 200:
        logging.info(f"map2result was sent")
    else:
        logging.error(f"{response.status} on sending map2result: {data}")
        raise RuntimeError(f"Not ok response: {response.status}")


def recv_game_result_list() -> str:
    response, content = httplib2.Http().request(ResultsHandlerLinks.GET_RES)
    games_data = content.decode("utf-8")
    logging.info(f"Aquired games data")
    return games_data
