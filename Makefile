NAME=env_turing

all: $(NAME)

re: clean
	make all

build:
	@if [ -z "$$(docker images -q $(NAME))" ]; then \
		docker build -t $(NAME) . ; \
	else \
		echo "$(NAME) image already exists."; \
	fi

$(NAME): build
	docker run -it $(NAME)

run: build
	docker run -it $(NAME) make run

clean:
	docker rmi -f $(NAME)

fclean: clean
	docker image prune -a
	docker builder prune -a


clean_docker:
	docker rm -vf $(docker ps -aq)
	docker rmi -f $(docker images -aq)

.PHONY: all re build $(NAME) clean fclean clean_docker run