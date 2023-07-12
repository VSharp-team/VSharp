import logging

import httplib2

from common.constants import ResultsHandlerLinks, WebsocketSourceLinks

from .classes import Agent2ResultsOnMaps


def aquire_ws() -> str:
    while True:
        response, content = httplib2.Http().request(WebsocketSourceLinks.GET_WS)
        aquired_ws_url = content.decode("utf-8")
        if aquired_ws_url == "":
            logging.warning(f"all sockets are in use")
            continue
        logging.info(f"aquired ws: {aquired_ws_url}")
        return aquired_ws_url


def return_ws(ws_url: str):
    logging.info(f"returning: {ws_url}")

    response, content = httplib2.Http().request(
        WebsocketSourceLinks.POST_WS,
        method="POST",
        body=ws_url,
    )

    if response.status == 200:
        logging.info(f"{ws_url} is returned")
    else:
        logging.error(f"{response.status} on returning {ws_url}")
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
