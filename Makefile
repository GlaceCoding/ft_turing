NAME=env_turing

all: $(NAME)

re: fclean
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
	echo "Nothing to rm"

fclean: clean

clean_docker:
	docker rm -vf $(docker ps -aq)
	docker rmi -f $(docker images -aq)

.PHONY: all re $(NAME) clean fclean clean_docker run