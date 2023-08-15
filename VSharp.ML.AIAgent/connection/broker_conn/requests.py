import json
import logging

import httplib2

from common.constants import ResultsHandlerLinks, WebsocketSourceLinks

from .classes import Agent2ResultsOnMaps, ServerInstanceInfo


def acquire_instance() -> ServerInstanceInfo:
    response, content = httplib2.Http().request(WebsocketSourceLinks.GET_WS)
    if response.status != 200:
        logging.error(f"{response.status} with {content=} on acuire_instance call")
        raise RuntimeError(f"Not ok response: {response}, {content}")
    aquired_instance = ServerInstanceInfo.from_json(json.loads(content.decode("utf-8")))
    logging.info(f"acquired ws: {aquired_instance}")
    return aquired_instance


def return_instance(instance: ServerInstanceInfo):
    logging.info(f"returning: {instance}")

    response, content = httplib2.Http().request(
        WebsocketSourceLinks.POST_WS,
        method="POST",
        body=instance.to_json(),
    )

    if response.status == 200:
        logging.info(f"{instance} is returned")
    else:
        logging.error(f"{response.status} on returning {instance}")
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
    logging.info(f"Acquired games data")
    return games_data
