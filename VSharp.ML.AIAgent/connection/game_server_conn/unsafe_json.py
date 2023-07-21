from dataclasses import asdict as dataclasses_asdict
from dataclasses import is_dataclass
from typing import ClassVar, Protocol


class Dataclass(Protocol):
    __dataclass_fields__: ClassVar[dict]


BareObject = type("BareObject", (), {})


def obj_from_dict(data: dict | list | str | float) -> BareObject:
    if isinstance(data, list):
        return [obj_from_dict(item) for item in data]
    if isinstance(data, dict):
        inner_dict = {
            field_name: obj_from_dict(field_raw)
            for field_name, field_raw in data.items()
        }
        bare_obj = BareObject()
        bare_obj.__dict__ = inner_dict
        return bare_obj
    return data


def asdict(
    data: BareObject | Dataclass | list | dict | str | float,
) -> dict | list | str | float:
    if isinstance(data, BareObject):
        return {asdict(k): asdict(v) for k, v in data.__dict__.items()}
    elif is_dataclass(data):
        try:
            return data.to_json()
        except TypeError:
            return asdict(dataclasses_asdict(data))
    elif isinstance(data, list):
        return [asdict(item) for item in data]
    elif isinstance(data, dict):
        return {asdict(k): asdict(v) for k, v in data.items()}
    else:
        return data
